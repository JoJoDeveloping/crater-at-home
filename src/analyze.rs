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
    sync::{Arc, OnceLock},
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
    time: f64,
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
enum RunResult {
    Success,
    Failure(FurtherClassificationResult),
    Timeout,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
enum ClassificationResult {
    MissingPartially,
    FilteredTimeout,
    Filtered(FurtherClassificationResult),
    Compared { tb: RunResult, sb: RunResult },
}

static BORROW_UB_REGEX: OnceLock<Regex> = OnceLock::new();

fn analyze_further(result: &TestResult) -> FurtherClassificationResult {
    let regex = BORROW_UB_REGEX.get_or_init(|| {
        Regex::new("(error: Undefined Behavior: Data race detected between .* retag (read|write)( of type `.[^`]*`|) on thread|help: this indicates a potential bug in the program: it performed an invalid operation, but the (Tree|Stacked) Borrows rules it violated are still experimental)").unwrap()
    });
    if regex.is_match(&result.stderr) {
        return FurtherClassificationResult::UndefinedBehaviorBorrow;
    } else if result
        .stderr
        .contains("thread 'rustc' has overflowed its stack\nfatal runtime error: stack overflow")
    {
        return FurtherClassificationResult::RustcStackOverflow;
    } else if result.stderr.contains("error: unsupported operation: ") {
        return FurtherClassificationResult::UnsupportedOperation;
    } else if result.stderr.contains("error: Undefined Behavior:") {
        return FurtherClassificationResult::UndefinedBehaviorOther;
    } else {
        return FurtherClassificationResult::Unknown;
    }
}

fn analyze_test_result_tbsb(res: &TestResult) -> RunResult {
    match res.kind {
        TestResultKind::Timeout => RunResult::Timeout,
        TestResultKind::Success => RunResult::Success,
        TestResultKind::Failure => RunResult::Failure(analyze_further(res)),
    }
}

fn analyze(
    nb_res: Option<&TestResult>,
    sb_res: Option<&TestResult>,
    tb_res: Option<&TestResult>,
    _args: &Args,
) -> (ClassificationResult, (f64, f64)) {
    let Some(nb_res) = nb_res else {
        return (ClassificationResult::MissingPartially, (0.0, 0.0));
    };
    let Some(sb_res) = sb_res else {
        return (ClassificationResult::MissingPartially, (0.0, 0.0));
    };
    let Some(tb_res) = tb_res else {
        return (ClassificationResult::MissingPartially, (0.0, 0.0));
    };
    (
        match nb_res.kind {
            TestResultKind::Timeout => ClassificationResult::FilteredTimeout,
            TestResultKind::Failure => ClassificationResult::Filtered(analyze_further(nb_res)),
            TestResultKind::Success => ClassificationResult::Compared {
                tb: analyze_test_result_tbsb(tb_res),
                sb: analyze_test_result_tbsb(sb_res),
            },
        },
        (sb_res.time, tb_res.time),
    )
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
    let mut crates_that_should_have_run: BTreeMap<_, _> = crates_that_should_have_run
        .iter()
        .enumerate()
        .map(|(idx, x)| (format!("{}@{}", x.name, x.version), idx))
        .collect();
    let mut crates_without_tests: BTreeSet<String> = BTreeSet::new();
    while let Some(entry) = entries.next_entry().await? {
        if entry.metadata().await?.is_dir() {
            let krate_name =
                String::from_utf8_lossy(entry.file_name().as_encoded_bytes()).to_string();
            if krate_name.chars().next().unwrap() == '.' {
                println!("Skipping spurious crate {krate_name}");
                continue;
            }
            if !args.partial && !crates_that_should_have_run.remove(&krate_name).is_some() {
                log::warn!("Krate {krate_name} was not supposed to have run!");
            }
            if exclude(&krate_name) {
                continue;
            }
            if !include(&krate_name) {
                continue;
            }
            count1 += 1;
        } else {
            println!("Skipping spurious file {entry:?}");
        }
    }
    let crates_missing_due_to_too_long = crates_that_should_have_run.len();
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
    let mut crates_with_tb_error = BTreeMap::new();
    let mut crates_with_sb_error = BTreeSet::new();
    let mut sb_total_time = 0.0;
    let mut tb_total_time = 0.0;
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
                let (ana, (sbdelta, tbdelta)) =
                    analyze(res_nb.get(&key), res_sb.get(&key), res_tb.get(&key), &args);
                tb_total_time += tbdelta;
                sb_total_time += sbdelta;
                match ana {
                    ClassificationResult::MissingPartially => {
                        to_rerun.insert(key.krate_name.clone());
                        if res_nb.len() == 0 || res_sb.len() == 0 || res_tb.len() == 0 {
                            pg.inc(1);
                            continue 'nextcrate;
                        }
                        log::warn!("Test {key} is only present in some test outputs!");
                    }
                    ClassificationResult::Compared {
                        tb: RunResult::Failure(k),
                        sb: RunResult::Success,
                    } => {
                        if matches!(k, FurtherClassificationResult::UndefinedBehaviorBorrow) {
                            *crates_with_tb_error
                                .entry(key.krate_name.clone())
                                .or_insert(0) += 1;
                        }
                        write_to_file(&res_tb, &key, ana_data_path, k, "TBTB").await?;
                        write_to_file(&res_sb, &key, ana_data_path, k, "SBforTB").await?
                    }

                    ClassificationResult::Compared {
                        tb: RunResult::Timeout,
                        sb: RunResult::Success,
                    } => {
                        // println!("Timeouting test: {key}");
                    }
                    ClassificationResult::Compared {
                        sb: RunResult::Failure(FurtherClassificationResult::UndefinedBehaviorBorrow),
                        tb: _k,
                    } => {
                        crates_with_sb_error.insert(key.krate_name.clone());
                        // if k == RunResult::Success {
                        //     write_to_file(
                        //         &res_sb,
                        //         &key,
                        //         ana_data_path,
                        //         FurtherClassificationResult::UndefinedBehaviorBorrow,
                        //         "SBSB",
                        //     )
                        //     .await?
                        // }
                    }
                    ClassificationResult::Compared {
                        tb: RunResult::Success,
                        sb: RunResult::Success,
                    } => {}
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
    if to_rerun.len() > 0 {
        log::warn!(
            "The following ({}) crates had spurious analysis bugs and need to be re-run: {:?}",
            to_rerun.len(),
            to_rerun
        );
    }
    let mut crates_that_should_have_run: BTreeSet<_> = crates_that_should_have_run
        .into_iter()
        .map(|(x, _)| x)
        .collect();
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
        total += *v;
    }
    println!(
        "Total: {total} tests from {crates_with_tests} crates, with {total_crates} tested in total (including crates without tests)"
    );
    let crates_with_tb_error_count_nofilter = crates_with_tb_error.len();
    {
        let total_aliasing_bugs: u32 = count_per_category
            .iter()
            .filter_map(|(k, v)| if is_aliasing_bug(*k) { Some(*v) } else { None })
            .sum();
        let fixed_by_tb = count_per_category.count(ClassificationResult::Compared {
            tb: RunResult::Success,
            sb: RunResult::Failure(FurtherClassificationResult::UndefinedBehaviorBorrow),
        });
        let broken_by_tb = count_per_category.count(ClassificationResult::Compared {
            sb: RunResult::Success,
            tb: RunResult::Failure(FurtherClassificationResult::UndefinedBehaviorBorrow),
        });
        let percent = 100f64 * (f64::from(fixed_by_tb) / f64::from(total_aliasing_bugs));
        let percent_broken = 100f64 * (f64::from(broken_by_tb) / f64::from(total_aliasing_bugs));
        println!("Tagline: Tree Borrows fixes {percent:.1}% of all aliasing bugs!!1! (Newly broken: {percent_broken:.3}% of aliasing bugs, found across only {} crates)", crates_with_tb_error_count_nofilter);
        println!("  These crates are: {crates_with_tb_error:?}");
        crates_with_tb_error
            .extract_if(|x, _| crates_with_sb_error.contains(x))
            .for_each(|x| drop(x));
        println!("  After filtering out those with SB errors as well: {crates_with_tb_error:?}");
    }
    let fancytable = print_fancy_table(
        &count_per_category,
        total_crates,
        crates_missing_due_to_too_long,
        crates_with_tb_error_count_nofilter,
    );
    File::create("experiment_results.tex")
        .await?
        .write_all(fancytable.as_bytes())
        .await?;
    println!("Timing: TB: {tb_total_time}    SB: {sb_total_time}");
    Ok(())
}

trait Foo {
    fn count(&self, data: ClassificationResult) -> u32;
}

impl Foo for BTreeMap<ClassificationResult, u32> {
    fn count(&self, data: ClassificationResult) -> u32 {
        self.get(&data).copied().unwrap_or_else(|| 0)
    }
}

fn print_fancy_table(
    res: &BTreeMap<ClassificationResult, u32>,
    total_crates: u64,
    crates_missing_due_to_too_long: usize,
    num_crates_tb_only: usize,
) -> String {
    use std::fmt::Write;
    let mut rs = String::new();
    writeln!(rs, "%THIS FILE IS AUTOGENERATED. DO NOT MODIFY\n\\newcommand{{\\pct}}[1]{{$#1\\%$}}\n\\newcolumntype{{R}}[2]{{%\n    >{{\\adjustbox{{angle=#1,lap=\\width-(#2),set height=2.5em,raise=-1em}}\\bgroup}}%\n    r%\n    <{{\\egroup}}%\n}}\n\\newcommand*\\rot{{\\multicolumn{{1}}{{R{{45}}{{1em}}}}}}% no optional argument here, please!").unwrap();
    writeln!(rs, "\\begin{{figure}}\\centering\\footnotesize").unwrap();
    let total: u32 = res.iter().map(|x| *x.1).sum();
    let passed_filtering;
    // we ignore these
    let total = total - res.count(ClassificationResult::MissingPartially);
    {
        let timeout = res.count(ClassificationResult::FilteredTimeout);
        let unsupported = res.count(ClassificationResult::Filtered(
            FurtherClassificationResult::UnsupportedOperation,
        ));
        let ub = res.count(ClassificationResult::Filtered(
            FurtherClassificationResult::UndefinedBehaviorOther,
        ));
        let ub_borrow = res.count(ClassificationResult::Filtered(
            FurtherClassificationResult::UndefinedBehaviorBorrow,
        ));
        assert_eq!(ub_borrow, 0);
        let rustc_overflow = res.count(ClassificationResult::Filtered(
            FurtherClassificationResult::RustcStackOverflow,
        ));
        assert_eq!(rustc_overflow, 0);
        let unknown = res.count(ClassificationResult::Filtered(
            FurtherClassificationResult::Unknown,
        ));
        let success: u32 = res
            .iter()
            .filter_map(|(res, num)| match res {
                ClassificationResult::Compared { .. } => Some(*num),
                _ => None,
            })
            .sum();
        assert_eq!(success, total - timeout - unsupported - ub - unknown);
        let timeout_pc = timeout as f64 / total as f64 * 100.0f64;
        let unsupported_pc = unsupported as f64 / total as f64 * 100.0f64;
        let ub_pc = ub as f64 / total as f64 * 100.0f64;
        let unknown_pc = unknown as f64 / total as f64 * 100.0f64;
        let success_pc = success as f64 / total as f64 * 100.0f64;
        let sum: u32 = timeout + unsupported + ub + unknown + success;
        let sum_pc = sum as f64 / total as f64 * 100.0f64;
        writeln!(rs, "\\begin{{subfigure}}[b]{{0.36\\textwidth}}\\centering\n\\begin{{tabular}}{{c|r|r}}\n\\textbf{{Cause}}&\\textbf{{Number}}&\\textbf{{Percent}}\\\\\\hline&&\\\\[-9pt]\n        Timeout&\\num{{{timeout}}}&\\pct{{{timeout_pc:.2}}}\\\\[2pt]\n        Unsupported&\\num{{{unsupported}}}&\\pct{{{unsupported_pc:.2}}}\\\\[2pt]\n        UB&\\num{{{ub}}}&\\pct{{{ub_pc:.2}}}\\\\[2pt]\n        Failure&\\num{{{unknown}}}&\\pct{{{unknown_pc:.2}}}\\\\[2pt]\n        Pass&\\num{{{success}}}&\\pct{{{success_pc:.2}}}\\\\[1pt]\\hline\n        $\\sum$&\\num{{{sum}}}&\\pct{{{sum_pc:.2}}}\\\\\n\\end{{tabular}}\n\\caption{{Filtering Run Results}}\n\\label{{fig:experiment-results-filtering}}\n\\end{{subfigure}}%").unwrap();
        passed_filtering = success;
    }
    writeln!(rs, "\\hfill%").unwrap();
    {
        let arr = [
            ("Pass", RunResult::Success),
            (
                "Borrow UB",
                RunResult::Failure(FurtherClassificationResult::UndefinedBehaviorBorrow),
            ),
            (
                "Other UB",
                RunResult::Failure(FurtherClassificationResult::UndefinedBehaviorOther),
            ),
            ("Timeout", RunResult::Timeout),
            (
                "Unsupported",
                RunResult::Failure(FurtherClassificationResult::UnsupportedOperation),
            ),
            (
                "Failure",
                RunResult::Failure(FurtherClassificationResult::Unknown),
            ),
            (
                "",
                RunResult::Failure(FurtherClassificationResult::RustcStackOverflow),
            ),
        ];
        write!(rs, "\\begin{{subfigure}}[b]{{0.61\\textwidth}}\\centering\\vspace{{1em}}\n\\begin{{tabular}}{{r|rrrrrr|r}}\n    \\diagbox[width=\\widthof{{Unsupported}}+1em,height=3em]{{\\textbf{{TB}}}}{{\\textbf{{SB}}}} ").unwrap();
        for (name, _) in arr {
            if arr.is_empty() {
                write!(rs, "& \\raisebox{{-0.8em}}{{$\\sum$}}").unwrap();
            } else {
                // if name == "Unsupp." {
                //     name = "Unsupported";
                // }
                write!(rs, "& \\rot{{{name}}}").unwrap();
            }
        }
        writeln!(rs, "\\\\\\hline").unwrap();
        for (name, tb) in arr {
            write!(rs, "{name}").unwrap();
            let mut sum = 0;
            for (name2, sb) in arr {
                let res = res.count(ClassificationResult::Compared { tb, sb });
                if name.is_empty() || name2.is_empty() {
                    assert_eq!(res, 0);
                } else {
                    write!(rs, "&\\num{{{res}}}").unwrap();
                }
                sum += res;
            }
            if !name.is_empty() {
                writeln!(rs, "&\\num{{{sum}}}\\\\").unwrap();
            }
        }
        {
            write!(rs, "\\hline$\\sum$").unwrap();
            let mut tot = 0;
            for (name, sb) in arr {
                let sum: u32 = res
                    .iter()
                    .filter_map(|(rr, cnt)| match rr {
                        ClassificationResult::Compared { sb: sb2, .. } if *sb2 == sb => Some(*cnt),
                        _ => None,
                    })
                    .sum();
                tot += sum;
                if name.is_empty() {
                    assert_eq!(sum, 0);
                } else {
                    write!(rs, "&\\num{{{sum}}}").unwrap();
                }
            }
            writeln!(rs, "&\\num{{{tot}}}").unwrap();
        }
        writeln!(rs, "\\end{{tabular}}\n\\caption{{Results of Comparing Tree (TB) and Stacked Borrows (SB)}}\n\\label{{fig:experiment-results-comparison}}\n\\end{{subfigure}}%").unwrap();
    }
    writeln!(rs, "\\caption{{Results of Running \\num{{{total}}} Tests From \\num{{{total_crates}}} Crates}}\n\\label{{fig:experiment-results}}\n\\end{{figure}}%").unwrap();
    writeln!(
        rs,
        "\\newcommand{{\\TBResultsCratesMissingTooLong}}{{\\num{{{crates_missing_due_to_too_long}}}}}%"
    )
    .unwrap();
    writeln!(
        rs,
        "\\newcommand{{\\TBResultsTotalTests}}{{\\num{{{total}}}}}%"
    )
    .unwrap();
    writeln!(
        rs,
        "\\newcommand{{\\TBResultsPassedFiltering}}{{\\num{{{passed_filtering}}}}}%"
    )
    .unwrap();
    writeln!(
        rs,
        "\\newcommand{{\\TBResultsPassedFilteringPct}}{{\\num{{{:.0}}}}}%",
        (100 * passed_filtering) as f64 / total as f64
    )
    .unwrap();
    let num_sb = res
        .iter()
        .filter_map(|(k, n)| match k {
            ClassificationResult::Compared {
                sb: RunResult::Failure(FurtherClassificationResult::UndefinedBehaviorBorrow),
                ..
            } => Some(*n),
            _ => None,
        })
        .sum::<u32>();
    let num_tb = res
        .iter()
        .filter_map(|(k, n)| match k {
            ClassificationResult::Compared {
                tb: RunResult::Failure(FurtherClassificationResult::UndefinedBehaviorBorrow),
                ..
            } => Some(*n),
            _ => None,
        })
        .sum::<u32>();
    let num_tb_only = res
        .get(&ClassificationResult::Compared {
            tb: RunResult::Failure(FurtherClassificationResult::UndefinedBehaviorBorrow),
            sb: RunResult::Success,
        })
        .copied()
        .unwrap_or(0);
    writeln!(
        rs,
        "\\newcommand{{\\TBFailStackedBorrowsSum}}{{\\num{{{num_sb}}}}}%"
    )
    .unwrap();
    writeln!(
        rs,
        "\\newcommand{{\\TBFailTreeBorrowsSum}}{{\\num{{{num_tb}}}}}%"
    )
    .unwrap();
    writeln!(
        rs,
        "\\newcommand{{\\TBFailTreeBorrowsOnly}}{{\\num{{{num_tb_only}}}}}%"
    )
    .unwrap();
    writeln!(
        rs,
        "\\newcommand{{\\TBFailStackedTreeReductionPct}}{{\\num{{{:.2}}}}}%",
        (100 * (num_sb - num_tb)) as f64 / num_sb as f64
    )
    .unwrap();
    writeln!(
        rs,
        "\\newcommand{{\\TBFailTreeBorrowsOnlyCrates}}{{\\num{{{num_crates_tb_only}}}}}%"
    )
    .unwrap();
    rs
}

fn get_test_result(i: &Node, time: f64) -> TestResult {
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
        time,
    }
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
                let runtime = test.attribute("time").unwrap();
                let runtime = f64::from_str(runtime).unwrap();
                let tr = get_test_result(&test, runtime);
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

async fn write_to_file(
    res: &HashMap<TestName, TestResult>,
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
    let ss = format!("TOOK TIME: {}\n", tb_res.time);
    file.write_all(ss.as_bytes()).await?;
    file.write_all(b"STDOUT: ###################\n").await?;
    file.write_all(&tb_res.stdout.as_bytes()).await?;
    file.write_all(b"STDERR: ###################\n").await?;
    file.write_all(&tb_res.stderr.as_bytes()).await?;
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
        ClassificationResult::Compared {
            sb: RunResult::Failure(FurtherClassificationResult::UndefinedBehaviorBorrow),
            tb: _,
        } => true,
        ClassificationResult::Compared {
            tb: RunResult::Failure(FurtherClassificationResult::UndefinedBehaviorBorrow),
            sb: _,
        } => true,
        _ => false,
    }
}
