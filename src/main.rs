use std::path::PathBuf;
use std::process::ExitCode;
use clap::Parser as ClapParser;
use density_function_lang;
use density_function_lang::compiler::lexer::Lexer;
use density_function_lang::compiler::parser::Parser;

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

fn main() -> ExitCode {
    let config: Config = Config::parse();

    let source = std::fs::read_to_string(config.input);
    let source = if let Ok(source) = source { source } else { return ExitCode::FAILURE };

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);
    let statements = parser.parse();

    if parser.had_error() {
        return ExitCode::FAILURE;
    }

    println!("{}", statements.iter().map(|stmt| format!("{:?}", stmt)).collect::<Vec<String>>().join("\n"));

    ExitCode::SUCCESS
}
