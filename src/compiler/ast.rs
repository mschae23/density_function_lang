use std::fmt::{Debug, Formatter};
use crate::compiler::lexer::Token;

#[derive(Clone)]
pub enum Stmt {
    Template {
        name: Token,
        args: Vec<Token>,
        expr: Expr,
    },

    Function {
        name: Token,
        expr: Expr,
    },

    Error,
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Template { name, args, expr } =>
                write!(f, "template {}({}) = {:?};", name.source(), args.iter()
                    .map(|arg| arg.source().to_owned())
                    .collect::<Vec<String>>().join(", "), expr),

            Stmt::Function { name, expr } =>
                write!(f, "function {} = {:?};", name.source(), expr),

            Stmt::Error => write!(f, "Error;")
        }
    }
}

#[derive(Clone)]
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
        name: Token,
        args: Vec<Expr>,
    },
    Member {
        receiver: Box<Expr>,
        name: Token,
    },

    Object(Vec<(Token, Expr)>),
    Array(Vec<Expr>),

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
            Expr::FunctionCall { receiver, name, args } => {
                write!(f, "(")?;

                if let Some(receiver) = receiver {
                    write!(f, "{:?}.", receiver)?;
                }

                write!(f, "{}({}))", name, args.iter()
                    .map(|expr| format!("{:?}", expr))
                    .collect::<Vec<String>>().join(", "))
            },
            Expr::Member { receiver, name } => write!(f, "({:?}.{})", receiver, name.source()),

            // Copied from Debug for JsonElement
            Expr::Object(fields) => write!(f, "{{{}}}", fields.iter()
                .map(|(key, field)| format!("{}: {:?}", key.source(), field)).collect::<Vec<String>>()
                .join(", ")),
            Expr::Array(elements) => write!(f, "[{}]", elements.iter()
                .map(|element| format!("{:?}", element)).collect::<Vec<String>>()
                .join(", ")),

            Expr::Error => write!(f, "Error"),
        }
    }
}

#[derive(Clone)]
pub enum JsonElement {
    ConstantFloat(f32),
    ConstantInt(i32),
    ConstantString(String),

    Object(Vec<(String, JsonElement)>),
    Array(Vec<JsonElement>),

    Error,
}

impl Debug for JsonElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            JsonElement::ConstantFloat(value) => write!(f, "{}", value),
            JsonElement::ConstantInt(value) => write!(f, "{}", value),
            JsonElement::ConstantString(value) => write!(f, "\"{}\"", value),
            JsonElement::Object(fields) => write!(f, "{{{}}}", fields.iter()
                .map(|(key, field)| format!("{}: {:?}", key, field)).collect::<Vec<String>>()
                .join(", ")),
            JsonElement::Array(elements) => write!(f, "[{}]", elements.iter()
                .map(|element| format!("{:?}", element)).collect::<Vec<String>>()
                .join(", ")),
            JsonElement::Error => write!(f, "Error"),
        }
    }
}
