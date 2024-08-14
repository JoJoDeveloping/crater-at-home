use crate::{client::Client, db_dump};
use clap::Parser;
use color_eyre::Result;
use std::{collections::HashMap, sync::Arc};

#[derive(Parser)]
pub struct Args {}

#[tokio::main]
pub async fn run(_args: Args) -> Result<()> {
    let client = Arc::new(Client::new().await?);

    log::info!("Updating the cached crates.io database dump");
    let (crates, data) = db_dump::download()?;
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
    let serialized = serde_json::to_string(&data).unwrap();
    client.write_persistent("metadata.json", serialized.as_bytes())?;

    Ok(())
}
