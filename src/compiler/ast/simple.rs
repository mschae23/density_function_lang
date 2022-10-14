use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::path::PathBuf;
use std::rc::{Rc, Weak};
use crate::compiler::ast::typed::{ExprType, TypedToken};

use crate::compiler::lexer::Token;

#[derive(Clone, Copy, PartialEq)]
pub enum VariableType {
    Simple,
    Inline,
    Export,
}

impl Debug for VariableType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableType::Simple => write!(f, "simple"),
            VariableType::Inline => write!(f, "inline"),
            VariableType::Export => write!(f, "export"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Decl {
    Template {
        name: Token,
        this: Option<TypedToken>,
        args: Vec<TypedToken>,
        return_type: ExprType,
        expr: TemplateExpr,
    },

    Variable {
        name: Token,
        expr_type: ExprType,
        expr: Expr,
        kind: VariableType,
    },

    Module {
        name: Token,
        statements: Vec<Decl>,
    },
    Include {
        path: Token,
        declarations: Vec<Decl>, // From a different file
    },
    Import {
        path: Vec<Token>,
        selector: Option<Vec<Token>>,
    },

    Error,
}

impl Debug for Decl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Template { name, this, args, expr, .. } =>
                write!(f, "template {}({}{}) = {:?};", name.source(),
                       this.as_ref().map(|this| format!("{}, ", this.token.source())).unwrap_or_else(String::new),
                       args.iter()
                           .map(|arg| arg.token.source().to_owned())
                           .collect::<Vec<String>>().join(", "), expr),

            Decl::Variable { name, expr, kind, .. } =>
                write!(f, "{:?} {} = {:?};", kind, name.source(), expr),

            Decl::Module { name, statements } =>
                write!(f, "module {} {{ {} }}", name.source(), statements.iter().map(|stmt| format!("{:?}", stmt))
                    .collect::<Vec<String>>().join(" ")),
            Decl::Include { path, declarations: _ } => write!(f, "include \"{}\";", path.source()),
            Decl::Import { path, selector } => write!(f, "{}.{}",
                                                      path.iter().map(|name| name.source().to_owned()).collect::<Vec<String>>().join("."),
                                                      selector.as_ref().map(|names| names.iter().map(|name| name.source().to_owned()).collect::<Vec<String>>().join(", "))
                                                          .map(|names| format!("{{{}}}", names)).unwrap_or_else(|| String::from("*"))),

            Decl::Error => write!(f, "Error;")
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum TemplateExpr {
    Block {
        expressions: Vec<TemplateExpr>,
        last: Box<TemplateExpr>,
    },
    If {
        token: Token,
        condition: Expr,
        then: Box<TemplateExpr>,
        otherwise: Box<TemplateExpr>,
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
            TemplateExpr::If { condition, then, otherwise, .. } =>
                write!(f, "if ({:?}) {:?} else {:?}", condition, *then, *otherwise),
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
    Index {
        receiver: Box<Expr>,
        operator: Token,
        index: Box<Expr>,
    },
    BuiltinFunctionCall {
        name: Token,
        args: Vec<Expr>,
    },
    BuiltinType(ExprType),

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
            Expr::Index { receiver, index, .. } => write!(f, "({:?}[{:?}])", receiver, index),
            Expr::BuiltinFunctionCall { name, args } => {
                write!(f, "(builtin.{}({}))", name.source(), args.iter()
                    .map(|expr| format!("{:?}", expr))
                    .collect::<Vec<String>>().join(", "))
            },
            Expr::BuiltinType(element_type) => write!(f, "{:?}", element_type),

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

#[derive(Debug, PartialEq)]
pub struct ExportFunction {
    pub name: String,
    pub path: PathBuf,
    pub json: JsonElement,
}

#[derive(Debug, PartialEq)]
pub struct CompiledExport {
    pub name: String,
    pub json: JsonElement,
}

#[derive(Debug)]
pub struct Template {
    pub name: String,
    // Can be identifiers or operator names (like `+`)
    pub receiver: bool,
    pub args: Vec<String>,
    pub expr: TemplateExpr,
    pub current_modules: Vec<Weak<RefCell<Module>>>,
    pub file_path: Rc<RefCell<PathBuf>>,
}

impl PartialEq for Template {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.receiver == other.receiver
            && self.args == other.args
            && self.expr == other.expr
            && self.file_path == other.file_path
    }
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: String,
    pub sub_modules: Vec<Rc<RefCell<Module>>>,
    pub templates: Vec<Rc<RefCell<Template>>>,
    pub exports: Vec<Rc<RefCell<CompiledExport>>>,

    pub imported_sub_modules: Vec<Rc<RefCell<Module>>>,
    pub imported_templates: Vec<Rc<RefCell<Template>>>,
    pub imported_exports: Vec<Rc<RefCell<CompiledExport>>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum JsonElementType {
    // @formatter:off
    Float, Int, Boolean, String,
    Object, Array,
    Module, Template,
    Type,
    Error
    // @formatter:on
}

impl Debug for JsonElementType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            JsonElementType::Float => write!(f, "float"),
            JsonElementType::Int => write!(f, "int"),
            JsonElementType::Boolean => write!(f, "boolean"),
            JsonElementType::String => write!(f, "string"),
            JsonElementType::Object => write!(f, "object"),
            JsonElementType::Array => write!(f, "array"),
            JsonElementType::Module => write!(f, "module"),
            JsonElementType::Template => write!(f, "template"),
            JsonElementType::Type => write!(f, "type"),
            JsonElementType::Error => write!(f, "error"),
        }
    }
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

    Type(JsonElementType),

    Error,
}

impl PartialEq for JsonElement {
    fn eq(&self, other: &Self) -> bool {
        match self {
            JsonElement::ConstantFloat(value) => match other {
                JsonElement::ConstantFloat(other) => value.eq(other),
                JsonElement::ConstantInt(other) => value.eq(&(*other as f64)),
                _ => false,
            },
            JsonElement::ConstantInt(value) => match other {
                JsonElement::ConstantFloat(other) => (&(*value as f64)).eq(other),
                JsonElement::ConstantInt(other) => value.eq(other),
                _ => false,
            },
            JsonElement::ConstantBoolean(value) => match other {
                JsonElement::ConstantBoolean(other) => value.eq(other),
                _ => false,
            },
            JsonElement::ConstantString(value) => match other {
                JsonElement::ConstantString(other) => value.eq(other),
                _ => false,
            },
            JsonElement::Object(fields) => match other {
                JsonElement::Object(other) => fields.eq(other),
                _ => false,
            },
            JsonElement::Array(elements) => match other {
                JsonElement::Array(other) => elements.eq(other),
                _ => false,
            },
            JsonElement::Module(_) => false,
            JsonElement::Template(_) => false,
            JsonElement::Type(value) => match other {
                JsonElement::Type(other) => value.eq(other),
                _ => false,
            },
            JsonElement::Error => false,
        }
    }
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
            JsonElement::Type(element_type) => write!(f, "builtin.type.{:?}", element_type),
            JsonElement::Error => write!(f, "Error"),
        }
    }
}

impl From<&JsonElement> for JsonElementType {
    fn from(element: &JsonElement) -> Self {
        match element {
            JsonElement::ConstantFloat(_) => JsonElementType::Float,
            JsonElement::ConstantInt(_) => JsonElementType::Int,
            JsonElement::ConstantBoolean(_) => JsonElementType::Boolean,
            JsonElement::ConstantString(_) => JsonElementType::String,
            JsonElement::Object(_) => JsonElementType::Object,
            JsonElement::Array(_) => JsonElementType::Array,
            JsonElement::Module(_) => JsonElementType::Module,
            JsonElement::Template(_) => JsonElementType::Template,
            JsonElement::Type(_) => JsonElementType::Type,
            JsonElement::Error => JsonElementType::Error,
        }
    }
}

impl From<JsonElement> for JsonElementType {
    fn from(element: JsonElement) -> Self {
        JsonElementType::from(&element)
    }
}
