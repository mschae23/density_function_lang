use std::fmt::{Debug, Formatter};
use crate::compiler::lexer::Token;

pub enum Stmt {
    Function {
        name: Token,
        expr: Expr,
    },

    Error,
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Function { name, expr } => write!(f, "function {} = {:?};", name.source(), expr),

            Stmt::Error => write!(f, "Error;")
        }
    }
}

pub enum Expr {
    ConstantFloat(f32),
    ConstantInt(i32),
    ConstantString(String),
    Identifier(Token),

    Group(Box<Expr>),

    UnaryOperator {
        operator: Token,
        expr: Box<Expr>,
    },
    BinaryOperator {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    FunctionCall {
        receiver: Option<Box<Expr>>,
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Member {
        receiver: Box<Expr>,
        name: Token,
    },

    Error,
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::ConstantFloat(value) => write!(f, "{}", value),
            Expr::ConstantInt(value) => write!(f, "{}", value),
            Expr::ConstantString(value) => write!(f, "\"{}\"", value),
            Expr::Identifier(value) => write!(f, "{}", value.source()),
            Expr::Group(expr) => write!(f, "{:?}", expr),
            Expr::UnaryOperator { operator, expr } => write!(f, "({}{:?})", operator.source(), expr),
            Expr::BinaryOperator { left, operator, right } => write!(f, "({:?} {} {:?})", left, operator.source(), right),
            Expr::FunctionCall { receiver, callee, args } => {
                write!(f, "(")?;

                if let Some(receiver) = receiver {
                    write!(f, "{:?}.", receiver)?;
                }

                write!(f, "{:?}({}))", callee, args.iter()
                    .map(|expr| format!("{:?}", expr))
                    .collect::<Vec<String>>().join(", "))
            },
            Expr::Member { receiver, name } => write!(f, "({:?}.{})", receiver, name.source()),
            Expr::Error => write!(f, "Error"),
        }
    }
}
