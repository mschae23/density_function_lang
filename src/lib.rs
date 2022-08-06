pub mod util;
pub mod compiler;

use std::cell::RefCell;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use clap::Parser as ClapParser;
use crate::compiler::ast::{Decl, ExportFunction};
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

    #[clap(long, help = "Disable pretty printing")]
    pub no_pretty_print: bool,
    #[clap(long, default_value = "    ", help = "Indentation string for pretty printing")]
    pub indentation: String,
}

pub fn parse(path: &Path) -> Result<Option<Vec<Decl>>, std::io::Error> {
    let source = std::fs::read_to_string(path.to_owned())?;

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer, path.to_owned());
    let statements = parser.parse();

    if parser.had_error() {
        return Ok(None);
    }

    Ok(Some(statements))
}

pub fn compile(path: PathBuf, target_dir: PathBuf, config: Rc<Config>) -> Result<Option<(Vec<Rc<RefCell<ExportFunction>>>, Compiler)>, std::io::Error> {
    let statements = match parse(&path)? {
        Some(result) => result,
        None => return Ok(None),
    };

    let mut compiler = Compiler::new(path.to_owned(), target_dir.to_owned(), config);
    compiler.compile(statements);
    let functions = compiler.collect_exports();

    if compiler.had_error() {
        return Ok(None);
    }

    Ok(Some((functions, compiler)))
}

pub fn run() -> Result<(), std::io::Error> {
    let config: Rc<Config> = Rc::new(Config::parse());

    let (functions, _) = match compile(config.input.to_owned(), config.target_dir.to_owned(), Rc::clone(&config))? {
        Some(result) => result,
        None => return Err(std::io::Error::from(std::io::ErrorKind::InvalidData)),
    };

    std::fs::create_dir_all(&config.target_dir)?;
    let mut writer = JsonWriter::new(config.indentation.to_owned(), !config.no_pretty_print);

    for function in functions {
        let function = &*function.borrow();

        if let Some(parent) = function.path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let mut file = std::fs::File::create(&function.path)?;

        writer.write_element(&function.json, &mut file)?;
        file.write_all(b"\n")?;
    }

    Ok(())
}
