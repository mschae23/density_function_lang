pub mod util;
pub mod compiler;

use std::cell::RefCell;
use std::io::Write;
use std::path::PathBuf;
use std::rc::Rc;
use clap::Parser as ClapParser;
use crate::compiler::ast::ExportFunction;
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

pub fn compile(path: PathBuf, target_dir: PathBuf, verbose: bool) -> Result<Option<(Vec<Rc<RefCell<ExportFunction>>>, Compiler)>, std::io::Error> {
    let source = std::fs::read_to_string(path.to_owned())?;

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer, path.to_owned());
    let statements = parser.parse();

    if parser.had_error() {
        return Ok(None);
    }

    let mut compiler = Compiler::new(path.to_owned(), target_dir.to_owned(), verbose);
    let functions = compiler.compile(statements);

    if compiler.had_error() {
        return Ok(None);
    }

    Ok(Some((functions, compiler)))
}

pub fn run() -> Result<(), std::io::Error> {
    let config: Config = Config::parse();

    let (functions, _) = match compile(config.input.to_owned(), config.target_dir.to_owned(), config.verbose)? {
        Some(result) => result,
        None => return Err(std::io::Error::from(std::io::ErrorKind::InvalidData)),
    };

    std::fs::create_dir_all(&config.target_dir)?;
    let mut writer = JsonWriter::new(String::from("    "), true);

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
