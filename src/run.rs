use crate::{client::Client, Crate, Version};
use clap::Parser;
use color_eyre::eyre::{eyre, Result};
use once_cell::sync::Lazy;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    process::Stdio,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};
use tokio::{
    io::{AsyncBufReadExt, AsyncWriteExt, BufReader},
    sync::Semaphore,
    task::JoinSet,
};
use uuid::Uuid;

static TEST_END_DELIMITER: Lazy<Uuid> = Lazy::new(Uuid::new_v4);

// These crates generate gigabytes of output then don't build.
const IGNORED_CRATES: &[&str] = &[
    "clacks_mtproto",
    "stdweb",
    "wayland-raw-protocol-bindings",
    "pleingres",
];

#[derive(Parser, Clone)]
pub struct Args {
    /// Run the top `n` most-recently-downloaded crates
    #[clap(long, conflicts_with = "crate_list")]
    crates: Option<usize>,

    /// A path to a file containing a whitespace-separated list of crates to run
    #[clap(long, conflicts_with = "crates")]
    crate_list: Option<String>,

    #[clap(long, default_value_t = 8)]
    memory_limit_gb: usize,

    #[clap(long, action)]
    no_push: bool,

    #[clap(long, action)]
    live_log: bool,

    #[clap(long)]
    jobs: Option<usize>,

    #[clap(long)]
    rev: bool,

    #[clap(
        long,
        default_value = "x86_64-unknown-linux-gnu",
        value_parser = clap::builder::PossibleValuesParser::new(["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"])
    )]
    target: String,
}

async fn build_crate_list(args: &Args, client: &Client) -> Result<(Vec<Crate>, bool)> {
    let mut rebuild_all = false;
    let all_crates = client.get_crate_versions().await?;
    let crates = if let Some(crate_list) = &args.crate_list {
        let crate_list = fs::read_to_string(crate_list).unwrap();
        let all_crates: HashMap<String, Crate> = all_crates
            .into_iter()
            .map(|c| (c.name.clone(), c))
            .collect();
        let mut crates = Vec::new();
        for line in crate_list.split_whitespace() {
            let mut it = line.split(|c| c == '@' || c == '/');
            let name = it.next().unwrap();
            let version = it.next();
            if let Some(c) = all_crates.get(name) {
                crates.push(Crate {
                    version: version
                        .map(Version::parse)
                        .unwrap_or_else(|| c.version.clone()),
                    ..c.clone()
                });
            }
        }
        crates.sort_by(|a, b| a.recent_downloads.cmp(&b.recent_downloads));
        rebuild_all = true;
        crates
    } else if let Some(crate_count) = args.crates {
        let mut crates = all_crates;
        crates.truncate(crate_count);

        crates
    } else {
        all_crates
    };
    Ok((crates, rebuild_all))
}

#[tokio::main]
pub async fn run(args: Args) -> Result<()> {
    let dockerfile = if std::env::var_os("CI").is_some() {
        "docker/Dockerfile.ci"
    } else {
        "docker/Dockerfile"
    };
    let status = std::process::Command::new("docker")
        .args(["build", "-t", "crater-at-home", "-f", dockerfile, "docker/"])
        .status()?;
    color_eyre::eyre::ensure!(status.success(), "docker image build failed!");

    log::info!("Figuring out what crates have a build log already");
    let client = Arc::new(Client::new().await?);
    let (mut crates, rebuild_all) = build_crate_list(&args, &client).await?;
    let total = crates.len();

    if !args.rev {
        // We are going to pop crates from this, so we now need to invert the order
        crates = crates.into_iter().rev().collect::<Vec<_>>();
    }
    let crates = Arc::new(Mutex::new(crates));
    let git_mutex = Arc::new(Semaphore::new(1));
    let last_push = Arc::new(Mutex::new(Instant::now()));

    let mut tasks = JoinSet::new();
    for cpu in 0..args.jobs.unwrap_or_else(num_cpus::get) {
        let crates = crates.clone();
        let args = args.clone();
        let _client = client.clone();

        let test_end_delimiter_with_dashes = format!("-{}-\n", *TEST_END_DELIMITER).into_bytes();

        let mut child = spawn_worker(&args, cpu);
        let git_mutex = git_mutex.clone();
        let last_push = last_push.clone();

        tasks.spawn(async move {
            loop {
                let mut stdout = BufReader::new(child.stdout.as_mut().unwrap());
                let (krate, numcrate) = {
                    let mut foo = crates.lock().unwrap();
                    let nxtcrate = match foo.pop() {
                        None => break,
                        Some(krate) => krate,
                    };
                    (nxtcrate, total - foo.len())
                };

                if IGNORED_CRATES.contains(&&krate.name[..]) {
                    continue;
                }

                if !rebuild_all && git_check_crate_committed(&git_mutex, &krate).await.unwrap() {
                    log::info!(
                        "Skipping {} {} as it is already committed",
                        krate.name,
                        krate.version
                    );
                    continue;
                }

                log::info!("Running {} {}", krate.name, krate.version);
                let start = Instant::now();

                child
                    .stdin
                    .as_mut()
                    .unwrap()
                    .write_all(format!("{}@{}\n", krate.name, krate.version).as_bytes())
                    .await
                    .unwrap();

                let mut output = Vec::new();
                loop {
                    let bytes_read = stdout.read_until(b'\n', &mut output).await.unwrap();
                    if output.ends_with(&test_end_delimiter_with_dashes) {
                        output.truncate(output.len() - test_end_delimiter_with_dashes.len() - 1);
                        break;
                    }
                    if bytes_read == 0 {
                        break;
                    }
                    if args.live_log {
                        // strip off last \n
                        let news = &output[output.len() - bytes_read..output.len() - 2];
                        let news = String::from_utf8_lossy(news);
                        log::info!("log for {}: {}", krate.name, news);
                    }
                }

                if let Ok(Some(_)) = child.try_wait() {
                    log::warn!("A worker crashed! Standing up a new one...");
                    child = spawn_worker(&args, cpu);
                    // Don't upload logs for crashed runs
                    continue;
                }

                let stop = Instant::now();

                log::info!(
                    "Uploading to Git server {} {} -- testing took {:?}",
                    krate.name,
                    krate.version,
                    stop.checked_duration_since(start)
                );

                save_and_push_logs(
                    &krate,
                    numcrate,
                    total,
                    &output,
                    git_mutex.clone(),
                    last_push.clone(),
                    &args,
                )
                .await
                .unwrap();

                log::info!(
                    "Finished {} {}, which was crate {} out of {}!",
                    krate.name,
                    krate.version,
                    numcrate,
                    total
                );
            }
        });
    }

    while let Some(task) = tasks.join_next().await {
        task?;
    }
    git_push(git_mutex, last_push, true, &args).await?;

    log::info!("done!");

    Ok(())
}

async fn save_and_push_logs(
    krate: &Crate,
    numcrate: usize,
    total: usize,
    output: &[u8],
    mutex: Arc<Semaphore>,
    lastpush: Arc<Mutex<Instant>>,
    args: &Args,
) -> Result<()> {
    let crate_base = format!("{}@{}", krate.name, krate.version);
    let mut file = File::create(format!("output/{crate_base}/global_log.txt"))?;
    file.write_all(output)?;
    drop(file);
    let lock = mutex.acquire().await?;
    let mut git_add = tokio::process::Command::new("git");
    git_add
        .args(["-C", "output/", "add", crate_base.as_str()])
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()?
        .wait()
        .await?;
    let mut git_commit = tokio::process::Command::new("git");
    git_commit
        .args([
            "-C",
            "output/",
            "commit",
            "--quiet",
            "--only",
            crate_base.as_str(),
            "-m",
            format!(
                "Run for {}, which is crate {}/{}",
                crate_base.as_str(),
                numcrate,
                total
            )
            .as_str(),
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()?
        .wait()
        .await?;
    drop(lock);
    git_push(mutex, lastpush, false, &args).await?;
    Ok(())
}

async fn git_push(
    mutex: Arc<Semaphore>,
    lastpush: Arc<Mutex<Instant>>,
    force: bool,
    args: &Args,
) -> Result<()> {
    let doit = force || {
        let mut mv = lastpush.lock().unwrap();
        if let Some(x) = Instant::now().checked_duration_since(*mv) {
            if x > Duration::from_secs(60) {
                *mv = Instant::now();
                true
            } else {
                false
            }
        } else {
            false
        }
    };
    if !doit {
        log::info!("skipping push since last one was recent..");
        return Ok(());
    }
    let lock = mutex.acquire().await?;
    if args.no_push {
        log::info!("no_push was specified, not actually pushing!");
    } else {
        let mut git_push = tokio::process::Command::new("git");
        git_push
            .args(["-C", "output/", "push"])
            .stdin(Stdio::piped())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()?
            .wait()
            .await?;
    }
    drop(lock);
    Ok(())
}

async fn git_check_crate_committed(mutex: &Arc<Semaphore>, krate: &Crate) -> Result<bool> {
    let crate_base = format!("{}@{}", krate.name, krate.version);
    let lock = mutex.acquire().await?;
    let mut git_add = tokio::process::Command::new("git");
    let status = git_add
        .args([
            "-C",
            "output/",
            "ls-files",
            "--error-unmatch",
            format!("{}/global_log.txt", crate_base.as_str()).as_str(),
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?
        .wait()
        .await?;
    drop(lock);
    match status.code() {
        Some(0) => Ok(true),
        Some(1) => Ok(false),
        _ => Err(eyre!("invalid return code!")),
    }
}

fn spawn_worker(args: &Args, cpu: usize) -> tokio::process::Child {
    let mut cmd = tokio::process::Command::new("docker");
    cmd.args([
        "run",
        "--rm",
        "--interactive",
        // Pin the build to a single CPU; this also ensures that anything doing
        // make -j $(nproc)
        // will not spawn processes appropriate for the host.
        &format!("--cpuset-cpus={cpu}"),
        // We set up our filesystem as read-only, but with 3 exceptions
        "--read-only",
        // The directory we are building in (not just its target dir!) is all writable
        "--volume=/build",
        // The directory we are building in (not just its target dir!) is all writable
        format!(
            "--mount=type=bind,source={},target=/output",
            std::env::current_dir()
                .unwrap()
                .join("output")
                .as_os_str()
                .to_str()
                .unwrap()
        )
        .as_str(),
        // rustdoc tries to write to and executes files in /tmp, odd move but whatever
        "--tmpfs=/tmp:exec",
        // The default cargo registry location; we download dependences in the sandbox
        "--tmpfs=/root/.cargo/registry",
        // cargo-miri builds a sysroot under /root/.cache, so why not make it all writeable
        "--tmpfs=/root/.cache:exec",
        &format!("--env=TEST_END_DELIMITER={}", *TEST_END_DELIMITER),
        &format!("--env=TARGET={}", args.target),
    ]);
    cmd.args([
        // Enforce the memory limit
        &format!("--memory={}g", args.memory_limit_gb),
        // Setting --memory-swap to the same value turns off swap
        &format!("--memory-swap={}g", args.memory_limit_gb),
        "crater-at-home:latest",
    ])
    .stdin(Stdio::piped())
    .stdout(Stdio::piped())
    .stderr(Stdio::inherit())
    .spawn()
    .unwrap()
}
