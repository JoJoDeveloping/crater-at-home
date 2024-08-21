use clap::Parser;
use color_eyre::eyre::{eyre, Result};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use rand::RngCore;
use regex::Regex;
use roxmltree::{Document, Node};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::Display,
    iter::once,
    path::PathBuf,
    process::Stdio,
    sync::Arc,
};
use tokio::{
    fs::{self, File},
    io::{AsyncReadExt, AsyncWriteExt},
};

use crate::{client::Client, Crate};

#[derive(Parser, Clone)]
pub struct Args {
    #[clap(long, action, help = "Do not `git pull` in the analysis folder.")]
    no_pull: bool,
    #[clap(
        long,
        action,
        help = "Usually, all tests that timeout w/o an aliasing model are discarded. But the two aliasing model runs do not timeout, we could \"recover\" and consider them in the analysis."
    )]
    // That sounded like a good idea, but the it's unclear whether these would be Filtered or FailBoth if both fail. So let's not do this. It's only 200 anyway.
    recover_timeout: bool,
    #[clap(long, action, help = "Add explainer text to each category.")]
    explain: bool,
    #[clap(
        long,
        action,
        help = "Many crates do not have tests. This flag will print the names some of these crates."
    )]
    show_some_without_tests: bool,
    #[clap(
        long,
        conflicts_with = "exclude",
        help = "A comma-separated (no spaces!) list of crates to consider, instead of all. Example: syn@1.0.0,foo@0.1.2"
    )]
    only: Option<String>,
    #[clap(
        long,
        conflicts_with = "only",
        help = "A comma-separated (no spaces!) list of crates to exclude. Format the same as --only."
    )]
    exclude: Option<String>,
    #[clap(long)]
    crates: Option<usize>,

    #[clap(
        help = "The folder storing the results (usually a git repository, if not use --no-pull)."
    )]
    folder: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TestResult {
    Success,
    Failure,
    Timeout,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct TestName {
    krate_name: String,
    testsuites_name: String, // most likely always "nextest-run"
    classname: String,
    testname: String,
}

impl Display for TestName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} / {} / {} / {}",
            self.krate_name, self.testsuites_name, self.classname, self.testname
        )
    }
}

#[derive(Debug)]
enum BorrowMode {
    NoBo,
    Stacks,
    Trees,
}

fn as_list<'b>(s: &'b Option<String>, default: bool) -> impl for<'a> Fn(&'a str) -> bool {
    let ohs: Option<HashSet<_>> = s
        .clone()
        .map(|x| x.split(',').map(|x| x.to_string()).collect());
    move |s| match &ohs {
        Option::None => default,
        Option::Some(x) => x.contains(s),
    }
}

impl BorrowMode {
    pub fn subdir_name(&self) -> &'static str {
        match self {
            BorrowMode::NoBo => "noborrows",
            BorrowMode::Stacks => "stackedborrows",
            BorrowMode::Trees => "treeborrows",
        }
    }

    pub fn subtree(&self, path: &PathBuf) -> PathBuf {
        path.join(self.subdir_name())
    }

    #[allow(unused)]
    pub fn kinds() -> impl Iterator<Item = BorrowMode> {
        once(BorrowMode::NoBo)
            .chain(once(BorrowMode::Stacks))
            .chain(once(BorrowMode::Trees))
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
enum ClassificationResult {
    MissingPartially,
    Filtered,
    FilteredCuriously,
    FilteredTimeout,
    FilteredTimeoutRecoverable,
    UnfilteredTimeout,
    SucceedAll,
    FailBoth,
    OnlyTrees,
    OnlyStacks,
}

impl ClassificationResult {
    pub fn describe(self) -> &'static str {
        match self {
            ClassificationResult::MissingPartially => "The test did not exist in some miri modes. This should not happen.",
            ClassificationResult::Filtered => "The test failed already without any aliasing model (due to e.g. foreign functions or int-to-ptr casts), we exclude it from analysis.",
            ClassificationResult::FilteredCuriously => "The test failed without any aliasing model, but it later worked _with_ an aliasing model. It is a flaky test and still excluded.",
            ClassificationResult::FilteredTimeout => "The test ran into a timeout without the aliasing model.",
            ClassificationResult::FilteredTimeoutRecoverable => "The test ran into a timeout without the aliasing model, but it finished in time with both aliasing models. Use --recover-timeout to include them in the analysis.",
            ClassificationResult::UnfilteredTimeout => "The test ran into a timeout _with_ one of the aliasing models, after succeeding before.",
            ClassificationResult::SucceedAll => "The test always succeeded. Yay.",
            ClassificationResult::FailBoth => "The test fails on both aliasing models.",
            ClassificationResult::OnlyTrees => "The test succeeded with Tree Borrows, but failed with Stacked Borrows. This number counts cases where TB is more permissive.",
            ClassificationResult::OnlyStacks => "The test succeeded with Stacked Borrows, but failed with Tree Borrows. These are interesting, since they exploit SB weirdness.",
        }
    }
}

fn analyze(
    nb_res: Option<&TestResult>,
    sb_res: Option<&TestResult>,
    tb_res: Option<&TestResult>,
    args: &Args,
) -> ClassificationResult {
    let Some(nb_res) = nb_res else {
        return ClassificationResult::MissingPartially;
    };
    let Some(sb_res) = sb_res else {
        return ClassificationResult::MissingPartially;
    };
    let Some(tb_res) = tb_res else {
        return ClassificationResult::MissingPartially;
    };
    match (nb_res, sb_res, tb_res) {
        (TestResult::Timeout, TestResult::Timeout, _)
        | (TestResult::Timeout, _, TestResult::Timeout) => ClassificationResult::FilteredTimeout,
        (TestResult::Timeout, _, _) if !args.recover_timeout => {
            ClassificationResult::FilteredTimeoutRecoverable
        }
        (TestResult::Success | TestResult::Timeout, TestResult::Success, TestResult::Success) => {
            ClassificationResult::SucceedAll
        }
        (TestResult::Success | TestResult::Timeout, TestResult::Success, TestResult::Failure) => {
            ClassificationResult::OnlyStacks
        }
        (TestResult::Success | TestResult::Timeout, TestResult::Failure, TestResult::Success) => {
            ClassificationResult::OnlyTrees
        }
        (TestResult::Success | TestResult::Timeout, TestResult::Failure, TestResult::Failure) => {
            ClassificationResult::FailBoth
        }
        (TestResult::Failure, a, b) => {
            if *a == TestResult::Success || *b == TestResult::Success {
                ClassificationResult::FilteredCuriously
            } else {
                ClassificationResult::Filtered
            }
        }
        (TestResult::Success, TestResult::Timeout, _)
        | (TestResult::Success, _, TestResult::Timeout) => ClassificationResult::UnfilteredTimeout,
    }
}

#[tokio::main]
pub async fn run(args: Args, multi: MultiProgress) -> Result<()> {
    let mut rng = rand::thread_rng();
    if !args.no_pull {
        git_pull(&args.folder).await?;
    }
    let mut entries = tokio::fs::read_dir(&args.folder).await?;
    let mut count1 = 0;
    let exclude = as_list(&args.exclude, false);
    let include = as_list(&args.only, true);
    let client = Arc::new(Client::new().await?);
    let crates_that_should_have_run = build_crate_list_discount(&args, &client).await?;
    let mut crates_that_should_have_run: BTreeSet<_> = crates_that_should_have_run
        .iter()
        .map(|x| format!("{}@{}", x.name, x.version))
        .collect();
    while let Some(entry) = entries.next_entry().await? {
        if entry.metadata().await?.is_dir() {
            let krate_name =
                String::from_utf8_lossy(entry.file_name().as_encoded_bytes()).to_string();
            if krate_name.chars().next().unwrap() == '.' {
                continue;
            }
            if !crates_that_should_have_run.remove(&krate_name) {
                log::warn!("Krate {krate_name} was not supposed to have run!");
            }
            if exclude(&krate_name) {
                continue;
            }
            if !include(&krate_name) {
                continue;
            }
            count1 += 1;
        }
    }
    if crates_that_should_have_run.len() > 0 {
        log::warn!(
            "The following ({}) crates are yet missing: {:?}",
            crates_that_should_have_run.len(),
            crates_that_should_have_run
        );
    }
    let pg: ProgressBar = multi.add(ProgressBar::new(count1));
    pg.set_message("Analyzing crates...");
    pg.set_style(
        ProgressStyle::with_template(
            "{msg} {spinner:.green} [{elapsed_precise}] [{wide_bar:.cyan/blue}] {percent_precise}% ({per_sec:.0}) ",
        )
        .unwrap()
        .progress_chars("#>-"),
    );
    let mut entries = tokio::fs::read_dir(&args.folder).await?;
    let mut rs = HashMap::new();
    let mut to_rerun = BTreeSet::new();
    let mut count_per_category = BTreeMap::new();
    let mut total_crates = 0;
    let mut crates_with_tests = 0;
    'nextcrate: while let Some(entry) = entries.next_entry().await? {
        if entry.metadata().await?.is_dir() {
            let krate_name =
                String::from_utf8_lossy(entry.file_name().as_encoded_bytes()).to_string();
            if exclude(&krate_name) {
                continue;
            }
            if !include(&krate_name) {
                continue;
            }
            if krate_name.chars().next().unwrap() == '.' {
                continue;
            }
            total_crates += 1;
            let path = entry.path();
            //log::info!("Processing {path:?}");
            let res_nb = read_junit_file(&BorrowMode::NoBo.subtree(&path), &krate_name).await?;
            let res_sb = read_junit_file(&BorrowMode::Stacks.subtree(&path), &krate_name).await?;
            let res_tb = read_junit_file(&BorrowMode::Trees.subtree(&path), &krate_name).await?;
            let mut keyset = HashSet::new();
            res_nb.keys().cloned().for_each(|x| {
                keyset.insert(x);
            });
            res_sb.keys().cloned().for_each(|x| {
                keyset.insert(x);
            });
            res_tb.keys().cloned().for_each(|x| {
                keyset.insert(x);
            });
            let mut hadtest = false;
            for key in keyset {
                let ana = analyze(res_nb.get(&key), res_sb.get(&key), res_tb.get(&key), &args);
                match ana {
                    ClassificationResult::MissingPartially => {
                        to_rerun.insert(key.krate_name.clone());
                        if res_nb.len() == 0 || res_sb.len() == 0 || res_tb.len() == 0 {
                            pg.inc(1);
                            continue 'nextcrate;
                        }
                        log::warn!("Test {key} is only present in some test outputs!");
                    }
                    ClassificationResult::FilteredCuriously => {
                        // log::warn!("Test {key} started succeeding after being filtered!");
                    }
                    ClassificationResult::OnlyStacks => {
                        // log::info!("Test {key} is OnlyStacks!");
                    }
                    _ => {}
                }
                rs.insert(key, ana);
                *count_per_category.entry(ana).or_insert(0) += 1;
                hadtest = true;
            }
            if hadtest {
                crates_with_tests += 1;
            } else {
                let should_have = should_have_tests(&path).await?;
                if should_have {
                    to_rerun.insert(krate_name.clone());
                    if args.show_some_without_tests {
                        log::warn!("No tests in crate {path:?} but things suggests there are!");
                    }
                } else if args.show_some_without_tests && rng.next_u32() % 500 == 4 {
                    log::warn!("No tests in crate {path:?}!");
                }
            }
            pg.inc(1);
        }
    }
    pg.finish_and_clear();
    if count1 != total_crates {
        log::error!("Number of crates changed in-between!");
    }
    // sometimes, copying the junit.xml file fails with
    if to_rerun.len() > 0 {
        log::warn!(
            "The following ({}) crates had spurious analysis bugs (missing junit.xml files) and need to be re-run: {:?}",
            to_rerun.len(),
            to_rerun
        );
    }
    crates_that_should_have_run.append(&mut to_rerun);
    if !crates_that_should_have_run.is_empty() {
        log::warn!("The file spurious_crates.txt has automatically been created to pass to a run.");
    }
    let mut newf = crates_that_should_have_run
        .into_iter()
        .collect::<Vec<_>>()
        .join("\n");
    newf.push('\n');
    File::create("spurious_crates.txt")
        .await?
        .write_all(newf.as_bytes())
        .await?;
    let mut total = 0;
    for (k, v) in &count_per_category {
        println!("Category {k:?}: {v}");
        if args.explain {
            println!("  Explanation for {k:?}: {}", k.describe());
        }
        total += *v;
    }
    println!(
        "Total: {total} tests from {crates_with_tests} crates, with {total_crates} tested in total (including crates without tests)"
    );
    {
        let total_aliasing_bugs: u32 = [
            ClassificationResult::OnlyStacks,
            ClassificationResult::OnlyTrees,
            ClassificationResult::FailBoth,
        ]
        .iter()
        .map(|x| count_per_category.get(&x).copied())
        .map(|x| x.unwrap_or(0))
        .sum();
        let fixed_by_tb = count_per_category
            .get(&ClassificationResult::OnlyTrees)
            .copied()
            .unwrap_or(0u32);
        let broken_by_tb = count_per_category
            .get(&ClassificationResult::OnlyStacks)
            .copied()
            .unwrap_or(0u32);
        let percent = 100f64 * (f64::from(fixed_by_tb) / f64::from(total_aliasing_bugs));
        let percent_broken = 100f64 * (f64::from(broken_by_tb) / f64::from(total_aliasing_bugs));
        println!("Tagline: Tree Borrows fixes {percent:.1}% of all aliasing bugs!!1! (Newly broken: {percent_broken:.3}% of aliasing bugs)");
    }
    Ok(())
}

fn get_test_result(i: &Node) -> TestResult {
    let mut res = TestResult::Success;
    for node in i.children() {
        if node.tag_name().name() == "failure" {
            let typ = node
                .attribute("type")
                .unwrap_or_else(|| panic!("Weird node {node:?}"));
            res = match typ {
                "test failure" => TestResult::Failure,
                "test timeout" => TestResult::Timeout,
                x => {
                    panic!("Unknown test result {x}")
                }
            }
        }
    }
    res
}

async fn read_junit_file(
    path: &PathBuf,
    krate_name: &String,
) -> Result<HashMap<TestName, TestResult>> {
    let path = path.join("junit.xml");
    if !tokio::fs::try_exists(&path).await? {
        return Ok(Default::default());
    }
    let mut file = File::open(path).await?;
    let mut vec = Vec::new();
    file.read_to_end(&mut vec).await?;
    let data = String::from_utf8_lossy(&vec);
    let doc = Document::parse(&data)?;
    let mut result = HashMap::new();
    for testsuites in doc.root().children() {
        if !testsuites.is_element() {
            continue;
        }
        let testsuites_name = testsuites.attribute("name").unwrap();
        assert_eq!(testsuites_name, "nextest-run");
        for testsuite in testsuites.children() {
            if !testsuite.is_element() {
                continue;
            }
            let testsuite_name = testsuite.attribute("name").unwrap();
            for test in testsuite.children() {
                if !test.is_element() {
                    continue;
                }
                let name = test.attribute("name").unwrap();
                let classname = test.attribute("classname").unwrap();
                assert_eq!(classname, testsuite_name);
                let tr = get_test_result(&test);
                let testname = TestName {
                    krate_name: krate_name.clone(),
                    testsuites_name: testsuites_name.to_string(),
                    classname: classname.to_string(),
                    testname: name.to_string(),
                };
                let hi = result.insert(testname.clone(), tr);
                if let Some(_) = hi {
                    panic!("Test {testname} occured twice!");
                }
            }
        }
    }

    Ok(result)
}

async fn should_have_tests(folder: &PathBuf) -> Result<bool> {
    let regex = Regex::new(
        r"Starting [1-9][0-9]* test(|s) across [0-9]+ binar(y|ies) \(run ID: [0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}, nextest profile: default-miri\)",
    ).unwrap();
    let path = folder.join("global_log.txt");
    if !tokio::fs::try_exists(&path).await? {
        return Err(eyre!("global_log not found in {folder:?}"));
    }
    let mut file = File::open(path).await?;
    let mut vec = Vec::new();
    file.read_to_end(&mut vec).await?;
    let data = String::from_utf8_lossy(&vec);
    return Ok(regex.is_match(data.as_ref()));
}

async fn git_pull(folder: &String) -> Result<()> {
    log::info!("Pulling {folder}");
    let mut git_pull = tokio::process::Command::new("git");
    let ec = git_pull
        .args(["-C", folder.as_str(), "pull", "--quiet"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()?
        .wait()
        .await?;
    if ec.success() {
        log::info!("Pulling successful!");
        Ok(())
    } else {
        Err(eyre!("git pull failed!"))
    }
}

async fn build_crate_list_discount(args: &Args, client: &Client) -> Result<Vec<Crate>> {
    let all_crates = client.get_crate_versions().await?;
    let crates = if let Some(crate_count) = args.crates {
        let mut crates = all_crates;
        crates.truncate(crate_count);

        crates
    } else {
        all_crates
    };
    Ok(crates)
}
