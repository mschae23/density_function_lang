use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::path::PathBuf;
use std::rc::{Rc, Weak};
use crate::compiler::lexer::Token;

#[derive(Clone, PartialEq)]
pub enum Stmt {
    Template {
        name: Token,
        this: Option<Token>,
        args: Vec<Token>,
        expr: TemplateExpr,
    },

    Export {
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
            Stmt::Template { name, this, args, expr } =>
                write!(f, "template {}({}{}) = {:?};", name.source(),
                    this.as_ref().map(|this| format!("{}, ", this.source())).unwrap_or_else(|| String::new()),
                    args.iter()
                        .map(|arg| arg.source().to_owned())
                        .collect::<Vec<String>>().join(", "), expr),

            Stmt::Export { name, expr } =>
                write!(f, "export {} = {:?};", name.source(), expr),

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
pub enum TemplateExpr {
    Block {
        expressions: Vec<TemplateExpr>,
        last: Box<TemplateExpr>,
    },
    Simple(Expr),
}

impl Debug for TemplateExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TemplateExpr::Block { expressions, last } =>
                write!(f, "{{ {}{:?} }}", expressions.iter()
                    .map(|expr| format!("{:?}; ", expr))
                    .collect::<Vec<String>>().join(""), *last),
            TemplateExpr::Simple(expr) => write!(f, "{:?}", expr),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Expr {
    ConstantFloat(f64),
    ConstantInt(i32),
    ConstantBoolean(bool),
    ConstantString(String),
    Identifier(Token),

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
        callee: Box<Expr>,
        token: Token,
        args: Vec<Expr>,
    },
    Member {
        receiver: Box<Expr>,
        name: Token,
    },
    BuiltinFunctionCall {
        name: Token,
        args: Vec<Expr>,
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
            Expr::ConstantBoolean(value) => write!(f, "{}", value),
            Expr::ConstantString(value) => write!(f, "\"{}\"", value),
            Expr::Identifier(value) => write!(f, "{}", value.source()),
            Expr::UnaryOperator { operator, expr } => write!(f, "({}{:?})", operator.source(), expr),
            Expr::BinaryOperator { left, operator, right } => write!(f, "({:?} {} {:?})", left, operator.source(), right),
            Expr::FunctionCall { callee, args, .. } => {
                write!(f, "({:?}({}))", callee, args.iter()
                    .map(|expr| format!("{:?}", expr))
                    .collect::<Vec<String>>().join(", "))
            },
            Expr::Member { receiver, name } => write!(f, "({:?}.{})", receiver, name.source()),
            Expr::BuiltinFunctionCall { name, args } => {
                write!(f, "(builtin.{}({}))", name.source(), args.iter()
                    .map(|expr| format!("{:?}", expr))
                    .collect::<Vec<String>>().join(", "))
            },

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
pub struct ExportFunction {
    pub name: String,
    pub path: PathBuf,
    pub json: JsonElement,
}

#[derive(Debug)]
pub struct Template {
    pub name: String, // Can be identifiers or operator names (like `+`)
    pub receiver: bool,
    pub args: Vec<String>,
    pub expr: TemplateExpr,
    pub current_modules: Vec<Weak<RefCell<Module>>>,
    pub file_path: Rc<RefCell<PathBuf>>,
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub sub_modules: Vec<Rc<RefCell<Module>>>,
    pub templates: Vec<Rc<RefCell<Template>>>,
    pub exports: Vec<Rc<RefCell<ExportFunction>>>,
}

#[derive(Clone)]
pub enum JsonElement {
    ConstantFloat(f64),
    ConstantInt(i32),
    ConstantBoolean(bool),
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
            JsonElement::ConstantBoolean(value) => write!(f, "{}", value),
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
