use super::*;

mod declarations {
    use crate::compiler::lexer::TokenType;
    use super::*;

    #[test]
    pub fn test_decl_1() {
        let mut type_checker = TypeChecker::new(PathBuf::from("main.test"), Rc::new(Config {
            input: PathBuf::from("main.test"),
            target_dir: PathBuf::from("target"),
            verbose: true,
            no_pretty_print: false,
            indentation: String::from("    ")
        }));

        let decl1 = Decl::Variable {
            name: Token::new(TokenType::Identifier, String::from("a"), TokenPos::new(1, 1), TokenPos::begin()),
            expr_type: ExprType::Any,
            expr: Expr::ConstantInt(4),
            kind: VariableType::Export,
        };

        assert_eq!(vec![TypedDecl::Variable(Rc::new(RefCell::new(TypedVariableDecl {
            name: Token::new(TokenType::Identifier, String::from("a"), TokenPos::new(1, 1), TokenPos::begin()),
            expr_type: ExprType::Int,
            expr: TypedExpr::ConstantInt(4),
            kind: VariableType::Export,
        })))], type_checker.check_types(vec![decl1]));
    }
}
