use density_function_language;
use density_function_language::compiler::lexer::Lexer;
use density_function_language::compiler::parser::Parser;

fn main() {
    let source = r"4.3333 * 8 + 99 + 2 + 3 * 67";

    let lexer = Lexer::new(&source);

    let mut parser = Parser::new(lexer);
    let expr = density_function_language::compiler::ast::Expr::Error; // parser.parse_expression();

    if parser.had_error() {
        eprintln!("Parser terminated with error.");
    }

    println!("{:?}", expr);
}
