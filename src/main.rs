use std::path::PathBuf;

use clap::Parser;

// TODO: probs better to add a 'run' subcommand (where 'config' becomes positional and -q and -f
// are specific to 'run') and convert 'init' into a subcommand.

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Run gitja with the specified config file.
    #[arg(short, long, default_value = "config.dhall", value_name = "FILE")]
    config: PathBuf,

    /// Suppress non-error output.
    #[arg(short, long)]
    quiet: bool,

    /// Force regeneration of all files.
    #[arg(short, long)]
    force: bool,

    /// Create a basic template with config in the current folder.
    #[arg(short, long)]
    init: bool,
}

fn main() {
    let cli = Cli::parse();

    println!("config: {:#?}", cli.config);
    println!("quiet: {:#?}", cli.quiet);
    println!("force: {:#?}", cli.force);
    println!("init: {:#?}", cli.init);
}
