pub enum Stmt {
    Fn {
        // TODO
    },
}

pub enum Expr {
    ConstantFloat(f32),

    UnaryOperator {
        operator: (),
        expr: Box<Expr>,
    },
    BinaryOperator {
        left: Box<Expr>,
        operator: (), // TODO TokenType
        right: Box<Expr>,
    },
    FunctionCall {
        receiver: Option<Box<Expr>>,
        name: String,
        args: Vec<Expr>,
    },
    Member {
        receiver: Box<Expr>,
        name: String,
    },

    Reference {
        // TODO
    }
}
