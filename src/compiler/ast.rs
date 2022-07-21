use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::path::PathBuf;
use std::rc::{Rc, Weak};
use crate::compiler::lexer::Token;

#[derive(Clone, PartialEq)]
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

    Module {
        name: Token,
        statements: Vec<Stmt>,
    },
    Include {
        path: Token,
    },
    Import {
        path: Vec<Token>,
        selector: Option<Vec<Token>>,
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

            Stmt::Module { name, statements } =>
                write!(f, "module {} {{ {} }}", name.source(), statements.iter().map(|stmt| format!("{:?}", stmt))
                    .collect::<Vec<String>>().join(" ")),
            Stmt::Include { path } => write!(f, "include \"{}\";", path.source()),
            Stmt::Import { path, selector } => write!(f, "{}.{}",
                path.iter().map(|name| name.source().to_owned()).collect::<Vec<String>>().join("."),
                selector.as_ref().map(|names| names.iter().map(|name| name.source().to_owned()).collect::<Vec<String>>().join(", "))
                    .map(|names| format!("{{{}}}", names)).unwrap_or_else(|| String::from("*"))),

            Stmt::Error => write!(f, "Error;")
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Expr {
    ConstantFloat(f64),
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

#[derive(Debug)]
pub struct OutputFunction {
    pub name: String,
    pub path: PathBuf,
    pub json: JsonElement,
}

#[derive(Debug)]
pub struct Template {
    pub name: String, // Can be identifiers or operator names (like `+`)
    pub args: Vec<String>,
    pub expr: Expr,
    pub current_modules: Vec<Weak<RefCell<Module>>>,
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub sub_modules: Vec<Rc<RefCell<Module>>>,
    pub templates: Vec<Rc<RefCell<Template>>>,
    pub output_functions: Vec<Rc<RefCell<OutputFunction>>>,
}

#[derive(Clone)]
pub enum JsonElement {
    ConstantFloat(f64),
    ConstantInt(i32),
    ConstantString(String),

    Object(Vec<(String, JsonElement)>),
    Array(Vec<JsonElement>),

    Module(Rc<RefCell<Module>>),
    Template(Rc<RefCell<Template>>),

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

            JsonElement::Module(module) => write!(f, "<module {}>", &module.borrow().name),
            JsonElement::Template(template) => write!(f, "<template {}>", &template.borrow().name),
            JsonElement::Error => write!(f, "Error"),
        }
    }
}
