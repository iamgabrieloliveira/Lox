mod lexer;

use clap::Parser;
use lexer::Lexer;
use std::{fs, path::PathBuf};

#[derive(Parser, Debug)]
struct Args {
    file: PathBuf,
}

impl Args {
    fn get_file_content(&self) -> String {
        fs::read_to_string(&self.file).unwrap()
    }
}

fn main() {
    let args = Args::parse();

    let content = args.get_file_content();

    let mut lexer = Lexer::new(content.as_str());

    lexer.scan_tokens();
    lexer.print_tokens();
}
