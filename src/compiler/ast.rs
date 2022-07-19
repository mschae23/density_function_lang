use std::fmt::{Debug, Formatter};
use crate::compiler::lexer::Token;

#[derive(Debug)]
pub enum Stmt {
    Fn {
        name: String,
        // TODO
    },
}

pub enum Expr {
    ConstantFloat(f32),
    ConstantInt(i32),

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
        name: String,
        args: Vec<Expr>,
    },
    Member {
        receiver: Box<Expr>,
        name: String,
    },

    Reference {
        namespace: Option<String>,
        path: String,
    },

    Error,
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::ConstantFloat(value) => write!(f, "{}", value),
            Expr::ConstantInt(value) => write!(f, "{}", value),
            Expr::Group(expr) => write!(f, "{:?}", expr),
            Expr::UnaryOperator { operator, expr } => write!(f, "({}{:?})", operator.source(), expr),
            Expr::BinaryOperator { left, operator, right } => write!(f, "({:?} {} {:?})", left, operator.source(), right),
            Expr::FunctionCall { receiver, name, args } => {
                if let Some(receiver) = receiver {
                    write!(f, "{:?}.", receiver)?;
                }

                write!(f, "{}({})", name, args.iter()
                    .map(|expr| format!("{:?}", expr))
                    .collect::<Vec<String>>().join(", "))
            },
            Expr::Member { receiver, name } => write!(f, "{:?}.{}", receiver, name),
            Expr::Reference { namespace, path } => {
                write!(f, "\"")?;

                if let Some(namespace) = namespace {
                    write!(f, "{}:", namespace)?;
                }

                write!(f, "{}\"", path)
            },
            Expr::Error => write!(f, "Error"),
        }
    }
}
