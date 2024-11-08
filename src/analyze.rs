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
    str::FromStr,
    sync::Arc,
};
use tokio::{
    fs::File,
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
    #[clap(
        long,
        conflicts_with = "partial",
        help = "Number of crates originally supposed to run, to identify missing crates"
    )]
    crates: Option<usize>,
    #[clap(
        long,
        action,
        help = "For looking at a partial run, does not try to find missing crates."
    )]
    partial: bool,

    #[clap(
        long,
        help = "The folder to store detailed extracts in.",
        default_value = "./analysis_results"
    )]
    analysis_results_folder: String,

    #[clap(
        help = "The folder storing the results (usually a git repository, if not use --no-pull)."
    )]
    folder: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
enum TestResultKind {
    Success,
    Failure,
    Timeout,
}

#[derive(Clone)]
struct TestResult {
    kind: TestResultKind,
    stdout: String,
    stderr: String,
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
enum FurtherClassificationResult {
    UndefinedBehaviorBorrow,
    UndefinedBehaviorOther,
    RustcStackOverflow,
    UnsupportedOperation,
    Unknown,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
enum UnfilteredTimeoutSource {
    TbOnly(i32),
    SbOnly,
    Both,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
enum ClassificationResult {
    MissingPartially,
    Filtered(FurtherClassificationResult),
    // FilteredCuriously,
    FilteredTimeout,
    // FilteredTimeoutRecoverable,
    UnfilteredTimeout(UnfilteredTimeoutSource),
    SucceedAll,
    FailBoth {
        sb: FurtherClassificationResult,
        tb: FurtherClassificationResult,
    },
    FailSbOnly(FurtherClassificationResult),
    FailTbOnly(FurtherClassificationResult),
}

impl ClassificationResult {
    pub fn describe(self) -> &'static str {
        match self {
            ClassificationResult::MissingPartially => "The test did not exist in some miri modes. This should not happen.",
            ClassificationResult::Filtered(_) => "The test failed already without any aliasing model (due to e.g. foreign functions or int-to-ptr casts), we exclude it from analysis.",
            // ClassificationResult::FilteredCuriously => "The test failed without any aliasing model, but it later worked _with_ an aliasing model. It is a flaky test and still excluded.",
            ClassificationResult::FilteredTimeout => "The test ran into a timeout without the aliasing model.",
            // ClassificationResult::FilteredTimeoutRecoverable => "The test ran into a timeout without the aliasing model, but it finished in time with both aliasing models. Use --recover-timeout to include them in the analysis.",
            ClassificationResult::UnfilteredTimeout(_) => "The test ran into a timeout _with_ one of the aliasing models, after succeeding before.",
            ClassificationResult::SucceedAll => "The test always succeeded. Yay.",
            ClassificationResult::FailBoth{..} => "The test fails on both aliasing models.",
            ClassificationResult::FailSbOnly(_) => "The test succeeded with Tree Borrows, but failed with Stacked Borrows. This number counts cases where TB is more permissive.",
            ClassificationResult::FailTbOnly(_) => "The test succeeded with Stacked Borrows, but failed with Tree Borrows. These are interesting, since they exploit SB weirdness.",
        }
    }
}

fn analyze_further(result: &TestResult) -> FurtherClassificationResult {
    let regex = Regex::new("error: Undefined Behavior: Data race detected between .* retag (read|write)( of type `.[^`]*`|) on thread").unwrap();
    if result.stderr.contains("help: this indicates a potential bug in the program: it performed an invalid operation, but the Tree Borrows rules it violated are still experimental") {
        return FurtherClassificationResult::UndefinedBehaviorBorrow;
    } else if result.stderr.contains("help: this indicates a potential bug in the program: it performed an invalid operation, but the Stacked Borrows rules it violated are still experimental") {
        return FurtherClassificationResult::UndefinedBehaviorBorrow;
    } else if regex.is_match(&result.stderr) {
        return FurtherClassificationResult::UndefinedBehaviorBorrow;
    }else if result.stderr.contains("thread 'rustc' has overflowed its stack\nfatal runtime error: stack overflow") {
        return FurtherClassificationResult::RustcStackOverflow;
    } else if result.stderr.contains("error: unsupported operation: ") {
        return FurtherClassificationResult::UnsupportedOperation;
    } else if result.stderr.contains("error: Undefined Behavior:") {
        return FurtherClassificationResult::UndefinedBehaviorOther
    } else {
        return FurtherClassificationResult::Unknown;
    }
}

fn analyze(
    nb_res: Option<&TestResult>,
    sb_res: Option<&TestResult>,
    tb_res: Option<&TestResult>,
    _args: &Args,
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
    match (nb_res.kind, sb_res.kind, tb_res.kind) {
        (TestResultKind::Timeout, _, _) => ClassificationResult::FilteredTimeout,
        (TestResultKind::Success, TestResultKind::Success, TestResultKind::Success) => {
            ClassificationResult::SucceedAll
        }
        (TestResultKind::Success, TestResultKind::Success, TestResultKind::Failure) => {
            ClassificationResult::FailTbOnly(analyze_further(tb_res))
        }
        (TestResultKind::Success, TestResultKind::Failure, TestResultKind::Success) => {
            ClassificationResult::FailSbOnly(analyze_further(sb_res))
        }
        (TestResultKind::Success, TestResultKind::Failure, TestResultKind::Failure) => {
            ClassificationResult::FailBoth {
                sb: analyze_further(sb_res),
                tb: analyze_further(tb_res),
            }
        }
        (TestResultKind::Failure, _a, _b) => {
            // if a == TestResultKind::Success || b == TestResultKind::Success {
            //     ClassificationResult::FilteredCuriously
            // } else {
            ClassificationResult::Filtered(analyze_further(nb_res))
            // }
        }
        (TestResultKind::Success, TestResultKind::Timeout, TestResultKind::Timeout) => {
            ClassificationResult::UnfilteredTimeout(UnfilteredTimeoutSource::Both)
        }
        (
            TestResultKind::Success,
            _k @ (TestResultKind::Success | TestResultKind::Failure),
            TestResultKind::Timeout,
        ) => ClassificationResult::UnfilteredTimeout(UnfilteredTimeoutSource::TbOnly(0)),
        (
            TestResultKind::Success,
            TestResultKind::Timeout,
            TestResultKind::Success | TestResultKind::Failure,
        ) => ClassificationResult::UnfilteredTimeout(UnfilteredTimeoutSource::SbOnly),
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
    let client = Arc::new(Client::new("output".into()).await?);
    let crates_that_should_have_run = build_crate_list_discount(&args, &client).await?;
    let mut crates_that_should_have_run: BTreeSet<_> = crates_that_should_have_run
        .iter()
        .map(|x| format!("{}@{}", x.name, x.version))
        .collect();
    let mut crates_without_tests: BTreeSet<String> = BTreeSet::new();
    while let Some(entry) = entries.next_entry().await? {
        if entry.metadata().await?.is_dir() {
            let krate_name =
                String::from_utf8_lossy(entry.file_name().as_encoded_bytes()).to_string();
            if krate_name.chars().next().unwrap() == '.' {
                continue;
            }
            if !args.partial && !crates_that_should_have_run.remove(&krate_name) {
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
    pg.set_message(format!("Analyzing {count1} crates"));
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
    let mut crates_with_tb_error = BTreeSet::new();
    let ana_data_path = &PathBuf::from_str(&args.analysis_results_folder)?;
    tokio::fs::create_dir_all(ana_data_path).await?;
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
            // log::info!("Processing {path:?}");
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
                let mut ana = analyze(
                    res_nb.get(&key).map(|x| &x.1),
                    res_sb.get(&key).map(|x| &x.1),
                    res_tb.get(&key).map(|x| &x.1),
                    &args,
                );
                match ana {
                    ClassificationResult::MissingPartially => {
                        to_rerun.insert(key.krate_name.clone());
                        if res_nb.len() == 0 || res_sb.len() == 0 || res_tb.len() == 0 {
                            pg.inc(1);
                            continue 'nextcrate;
                        }
                        log::warn!("Test {key} is only present in some test outputs!");
                    }
                    // ClassificationResult::FilteredCuriously => {
                    //     log::warn!("Test {key} started succeeding after being filtered!");
                    // }
                    ClassificationResult::FailBoth { sb, tb } if tb != sb => {
                        to_rerun.insert(key.krate_name.clone());
                        // log::warn!(
                        //     "Test {key} behaved incongruent on TB ({tb:?}) and SB ({sb:?})!"
                        // );
                    }
                    ClassificationResult::FailTbOnly(k) => {
                        if matches!(k, FurtherClassificationResult::UndefinedBehaviorBorrow) {
                            crates_with_tb_error.insert(key.krate_name.clone());
                        }
                        write_to_file(&res_tb, &key, ana_data_path, k, "TBTB").await?
                    }
                    ClassificationResult::FailSbOnly(
                        k @ FurtherClassificationResult::UndefinedBehaviorOther,
                    ) => write_to_file(&res_sb, &key, ana_data_path, k, "SBSB").await?,
                    ClassificationResult::UnfilteredTimeout(UnfilteredTimeoutSource::TbOnly(
                        ref mut _x,
                    )) => {
                        // *x = res_nb.get(&key).unwrap().0 as i32;
                        // if *x < 10 {
                        //     log::warn!("TB very slow {x} on test {key}!")
                        // }
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
                } else {
                    crates_without_tests.insert(krate_name);
                    if args.show_some_without_tests && rng.next_u32() % 500 == 4 {
                        log::warn!("No tests in crate {path:?}!");
                    }
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
    let mut newf = crates_without_tests
        .into_iter()
        .collect::<Vec<_>>()
        .join("\n");
    newf.push('\n');
    File::create("crates_without_tests.txt")
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
        let total_aliasing_bugs: u32 = count_per_category
            .iter()
            .filter_map(|(k, v)| if is_aliasing_bug(*k) { Some(*v) } else { None })
            .sum();
        let fixed_by_tb = count_per_category
            .get(&ClassificationResult::FailSbOnly(
                FurtherClassificationResult::UndefinedBehaviorBorrow,
            ))
            .copied()
            .unwrap_or(0u32);
        let broken_by_tb = count_per_category
            .get(&ClassificationResult::FailTbOnly(
                FurtherClassificationResult::UndefinedBehaviorBorrow,
            ))
            .copied()
            .unwrap_or(0u32);
        let percent = 100f64 * (f64::from(fixed_by_tb) / f64::from(total_aliasing_bugs));
        let percent_broken = 100f64 * (f64::from(broken_by_tb) / f64::from(total_aliasing_bugs));
        println!("Tagline: Tree Borrows fixes {percent:.1}% of all aliasing bugs!!1! (Newly broken: {percent_broken:.3}% of aliasing bugs, found across only {} crates)", crates_with_tb_error.len());
        println!("  These crates are: {crates_with_tb_error:?}");
    }
    Ok(())
}

fn get_test_result(i: &Node) -> TestResult {
    let mut res = TestResultKind::Success;
    let mut system_out = String::new();
    let mut system_err = String::new();
    for node in i.children() {
        match node.tag_name().name() {
            "failure" => {
                let typ = node
                    .attribute("type")
                    .unwrap_or_else(|| panic!("Weird node {node:?}"));
                res = match typ {
                    "test failure" => TestResultKind::Failure,
                    "test timeout" => TestResultKind::Timeout,
                    x => {
                        panic!("Unknown test result {x}")
                    }
                }
            }
            "system-err" => {
                for ele in node.children() {
                    system_err.push_str(ele.text().unwrap())
                }
            }
            "system-out" => {
                for ele in node.children() {
                    system_out.push_str(ele.text().unwrap())
                }
            }
            s if s.chars().all(char::is_whitespace) => {}
            s => panic!("Spurious tag \"{s}\": {node:?}"),
        }
    }
    TestResult {
        kind: res,
        stdout: system_out,
        stderr: system_err,
    }
}

async fn read_junit_file(
    path: &PathBuf,
    krate_name: &String,
) -> Result<HashMap<TestName, (f32, TestResult)>> {
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
                let runtime = test.attribute("time").unwrap();
                let runtime = f32::from_str(runtime).unwrap();
                let tr = get_test_result(&test);
                let testname = TestName {
                    krate_name: krate_name.clone(),
                    testsuites_name: testsuites_name.to_string(),
                    classname: classname.to_string(),
                    testname: name.to_string(),
                };
                let hi = result.insert(testname.clone(), (runtime, tr));
                if let Some(_) = hi {
                    panic!("Test {testname} occured twice!");
                }
            }
        }
    }

    Ok(result)
}

async fn write_to_file(
    res: &HashMap<TestName, (f32, TestResult)>,
    key: &TestName,
    path: &PathBuf,
    k: FurtherClassificationResult,
    prefix: &str,
) -> Result<()> {
    let tb_res = res.get(&key).unwrap();
    let path = path.join(
        format!(
            "{}{k:?}+{}+{}+{}+{}",
            prefix, key.krate_name, key.testsuites_name, key.classname, key.testname
        )
        .replace("/", ":"),
    );
    let mut file = tokio::fs::File::create(path).await?;
    let ss = format!("TOOK TIME: {}\n", tb_res.0);
    file.write_all(ss.as_bytes()).await?;
    file.write_all(b"STDOUT: ###################\n").await?;
    file.write_all(&tb_res.1.stdout.as_bytes()).await?;
    file.write_all(b"STDERR: ###################\n").await?;
    file.write_all(&tb_res.1.stderr.as_bytes()).await?;
    drop(file);
    Ok(())
}

async fn should_have_tests(folder: &PathBuf) -> Result<bool> {
    let regex = Regex::new(
        r"Starting [1-9][0-9]* test(|s) across [0-9]+ binar(y|ies) \(run ID: [0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}, nextest profile: default-miri\)",
    ).unwrap();
    let path = folder.join("global_log.txt");
    if !tokio::fs::try_exists(&path).await? {
        return Ok(false);
        // return Err(eyre!("global_log not found in {folder:?}"));
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
    if args.partial {
        return Ok(vec![]);
    }
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

fn is_aliasing_bug(x: ClassificationResult) -> bool {
    match x {
        ClassificationResult::FailBoth {
            sb: FurtherClassificationResult::UndefinedBehaviorBorrow,
            ..
        } => true,
        ClassificationResult::FailBoth {
            tb: FurtherClassificationResult::UndefinedBehaviorBorrow,
            ..
        } => true,
        ClassificationResult::FailTbOnly(FurtherClassificationResult::UndefinedBehaviorBorrow) => {
            true
        }
        ClassificationResult::FailSbOnly(FurtherClassificationResult::UndefinedBehaviorBorrow) => {
            true
        }
        ClassificationResult::SucceedAll => false,
        _ => false,
    }
}
