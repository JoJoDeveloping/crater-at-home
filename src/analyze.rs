use clap::Parser;
use color_eyre::eyre::Result;
use roxmltree::{Document, Node};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Display,
    iter::once,
    path::PathBuf,
};
use tokio::{
    fs::File,
    io::{AsyncReadExt, AsyncWriteExt},
};

#[derive(Parser, Clone)]
pub struct Args {
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
    UnfilteredTimeout,
    SucceedAll,
    FailBoth,
    OnlyTrees,
    OnlyStacks,
}

fn analyze(
    nb_res: Option<&TestResult>,
    sb_res: Option<&TestResult>,
    tb_res: Option<&TestResult>,
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
        (TestResult::Timeout, _, _) => ClassificationResult::FilteredTimeout,
        (TestResult::Failure, a, b) => {
            if *a == TestResult::Success || *b == TestResult::Success {
                ClassificationResult::FilteredCuriously
            } else {
                ClassificationResult::Filtered
            }
        }
        (TestResult::Success, TestResult::Timeout, _)
        | (TestResult::Success, _, TestResult::Timeout) => ClassificationResult::UnfilteredTimeout,
        (TestResult::Success, TestResult::Success, TestResult::Success) => {
            ClassificationResult::SucceedAll
        }
        (TestResult::Success, TestResult::Success, TestResult::Failure) => {
            ClassificationResult::OnlyStacks
        }
        (TestResult::Success, TestResult::Failure, TestResult::Success) => {
            ClassificationResult::OnlyTrees
        }
        (TestResult::Success, TestResult::Failure, TestResult::Failure) => {
            ClassificationResult::FailBoth
        }
    }
}

#[tokio::main]
pub async fn run(args: Args) -> Result<()> {
    let mut entries = tokio::fs::read_dir(args.folder).await?;
    let mut rs = HashMap::new();
    let mut to_rerun = HashSet::new();
    let mut count_per_category = BTreeMap::new();
    'nextcrate: while let Some(entry) = entries.next_entry().await? {
        if entry.metadata().await?.is_dir() {
            let krate_name =
                String::from_utf8_lossy(entry.file_name().as_encoded_bytes()).to_string();
            if krate_name.chars().next().unwrap() == '.' {
                continue;
            }
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
            for key in keyset {
                let ana = analyze(res_nb.get(&key), res_sb.get(&key), res_tb.get(&key));
                match ana {
                    ClassificationResult::MissingPartially => {
                        if res_nb.len() == 0 || res_sb.len() == 0 || res_tb.len() == 0 {
                            to_rerun.insert(key.krate_name);
                            continue 'nextcrate;
                        }
                        log::warn!("Test {key} is only present in some test outputs!");
                    }
                    ClassificationResult::FilteredCuriously => {
                        log::warn!("Test {key} started succeeding after being filtered!");
                    }
                    _ => {}
                }
                rs.insert(key, ana);
                *count_per_category.entry(ana).or_insert(0) += 1;
            }
        }
    }
    // sometimes, copying the junit.xml file fails with
    log::warn!(
        "The following ({}) crates had spurious analysis bugs (missing junit.xml files) and need to be re-run: {:?}",
        to_rerun.len(),
        to_rerun
    );
    let newf = to_rerun.into_iter().collect::<Vec<_>>().join("\n");
    File::create("spurious_crates.txt")
        .await?
        .write_all(newf.as_bytes())
        .await?;
    let mut total = 0;
    for (k, v) in &count_per_category {
        println!("Category {k:?}: {v}");
        total += *v;
    }
    println!("Total: {total}");
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
