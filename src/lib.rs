pub mod util;
pub mod compiler;

use std::io::Write;
use std::path::PathBuf;
use clap::Parser as ClapParser;
use crate::compiler::compiler::Compiler;
use crate::compiler::lexer::Lexer;
use crate::compiler::parser::Parser;
use crate::compiler::writer::JsonWriter;

#[derive(ClapParser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Config {
    #[clap(default_value = "main.densityfunction", help = "Main input file")]
    pub input: PathBuf,
    #[clap(long, default_value = "target", help = "Target directory")]
    pub target_dir: PathBuf,

    #[clap(short, long, help = "Print verbose log output")]
    pub verbose: bool,
}

pub fn run() -> Result<(), std::io::Error> {
    let config: Config = Config::parse();

    let source = std::fs::read_to_string(config.input)?;
    std::fs::create_dir_all(&config.target_dir)?;

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);
    let statements = parser.parse();

    if parser.had_error() {
        return Err(std::io::Error::from(std::io::ErrorKind::InvalidData));
    }

    let compiler = Compiler::new();
    let (functions, had_error) = compiler.compile(statements);

    if had_error {
        return Err(std::io::Error::from(std::io::ErrorKind::InvalidData));
    }

    let mut writer = JsonWriter::new(String::from("    "), true);

    let mut path = config.target_dir.clone();

    for (name, element) in functions {
        path.push(name);
        path.set_extension("json");

        let mut file = std::fs::File::create(&path)?;

        writer.write_element(element, &mut file)?;
        file.write_all(b"\n")?;

        path.pop();
    }

    Ok(())
}
