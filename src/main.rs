use std::io::Write;
use std::path::PathBuf;
use clap::Parser as ClapParser;
use density_function_lang;
use density_function_lang::compiler::compiler::Compiler;
use density_function_lang::compiler::lexer::Lexer;
use density_function_lang::compiler::parser::Parser;
use density_function_lang::compiler::writer::JsonWriter;

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

fn main() -> proc_exit::ExitResult {
    let config: Config = Config::parse();

    let source = std::fs::read_to_string(config.input)?;
    std::fs::create_dir_all(&config.target_dir)?;

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);
    let statements = parser.parse();

    if parser.had_error() {
        return proc_exit::Code::DATA_ERR.ok();
    }

    let compiler = Compiler::new();
    let functions = compiler.compile(statements);

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

    proc_exit::Code::SUCCESS.ok()
}
