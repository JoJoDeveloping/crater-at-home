use crate::{client::Client, db_dump, Crate};
use aws_smithy_types_convert::date_time::DateTimeExt;
use clap::Parser;
use color_eyre::{Report, Result};
use std::{collections::HashMap, fmt::Write, sync::Arc};
use tokio::{sync::Mutex, sync::Semaphore, task::JoinSet};

#[derive(Parser)]
pub struct Args {}

#[tokio::main]
pub async fn run(args: Args) -> Result<()> {
    let client = Arc::new(Client::new().await?);

    log::info!("Updating the cached crates.io database dump");
    let crates = db_dump::download()?;
    let mut name_to_downloads = HashMap::new();
    let mut versions = Vec::new();
    for krate in crates.iter() {
        name_to_downloads.insert(krate.name.clone(), krate.recent_downloads);
        versions.push((krate.name.clone(), krate.version.to_string()));
    }

    let serialized = serde_json::to_string(&versions).unwrap();
    client.write_persistent("crates.json", serialized.as_bytes())?;
    let serialized = serde_json::to_string(&name_to_downloads).unwrap();
    client.write_persistent("downloads.json", serialized.as_bytes())?;

    Ok(())
}
