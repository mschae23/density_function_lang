use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use clap::arg;
use crate::compiler::ast::simple::VariableType;
use crate::compiler::lexer::Token;

#[derive(Clone, PartialEq, Eq)]
pub enum ExprType {
    // @formatter:off
    Float, Int, Boolean, String,
    Object, Array,
    Type,
    Template {
        this: Option<ExprType>,
        args: Vec<ExprType>,
        return_type: ExprType,
    },
    Module,
    Any,
    Error,
    // @formatter:on
}

impl ExprType {
    pub fn can_coerce_to(&self, other: &ExprType) -> bool {
        if *self == ExprType::Error || *other == ExprType::Error {
            return false;
        }

        if *self == *other {
            return true;
        }

        if *other == ExprType::Any {
            return true;
        }

        if *self == ExprType::Int && *other == ExprType::Float {
            return true;
        }

        false
    }
}

impl Debug for ExprType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprType::Float => write!(f, "float"),
            ExprType::Int => write!(f, "int"),
            ExprType::Boolean => write!(f, "boolean"),
            ExprType::String => write!(f, "string"),
            ExprType::Object => write!(f, "object"),
            ExprType::Array => write!(f, "array"),
            ExprType::Type => write!(f, "type"),
            ExprType::Template { this, args, return_type } =>
                write!(f, "({}): {:?}", this.iter()
                    .map(|this_type| format!("this: {:?}", this_type))
                    .chain(args.iter().map(|arg_type| format!("{:?}", arg_type)))
                    .fold(String::new(), |acc, arg| format!("{}, {}", acc, arg)), return_type),
            ExprType::Module => write!(f, "module"),
            ExprType::Any => write!(f, "_"),
            ExprType::Error => write!(f, "error"),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct TypedToken {
    pub expr_type: ExprType,
    pub token: Token,
}

impl Debug for TypedToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.token, self.expr_type)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct TemplateDeclaration {
    pub name: Token,
    pub this: Option<TypedToken>,
    pub args: Vec<TypedToken>,
    pub return_type: ExprType,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ModuleDeclaration {
    pub name: Token,
}

#[derive(Clone, PartialEq, Debug)]
pub struct VariableDeclaration {
    pub name: Token,
    pub kind: VariableType,
    pub expr_type: ExprType,
}


#[derive(Clone, PartialEq)]
pub struct TypedTemplateDecl {
    pub name: Token,
    pub this: Option<TypedToken>,
    pub args: Vec<TypedToken>,
    pub return_type: ExprType,
    pub expr: TypedTemplateExpr,
}

impl Debug for TypedTemplateDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<type-checked template declaration>")
    }
}

#[derive(Clone, PartialEq)]
pub struct TypedModuleDecl {
    pub name: Token,
    pub declarations: Vec<TypedDecl>,
}

impl Debug for TypedModuleDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<type-checked module declaration>")
    }
}

#[derive(Clone, PartialEq)]
pub struct TypedVariableDecl {
    pub name: Token,
    pub expr_type: ExprType,
    pub expr: TypedExpr,
    pub kind: VariableType,
}

impl Debug for TypedVariableDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<type-checked variable declaration>")
    }
}

#[derive(Clone, PartialEq)]
pub enum TypedImportableDecl {
    Template(Rc<RefCell<TypedTemplateDecl>>),
    Module(Rc<RefCell<TypedModuleDecl>>),
    Variable(Rc<RefCell<TypedVariableDecl>>),
}

impl Debug for TypedImportableDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<type-checked importable declaration>")
    }
}

#[derive(Clone, PartialEq)]
pub enum TypedDecl {
    Template(Rc<RefCell<TypedTemplateDecl>>),
    Variable(Rc<RefCell<TypedVariableDecl>>),

    Module(Rc<RefCell<TypedModuleDecl>>),
    Include {
        path: Token,
        declarations: Vec<TypedDecl>,
    },
    Import {
        from_module: Rc<RefCell<TypedModuleDecl>>,
        imported: Vec<TypedImportableDecl>,
    },

    Error,
}

impl Debug for TypedDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<type-checked decl>")
    }
}

#[derive(Clone, PartialEq)]
pub enum TypedTemplateExpr {
    Block {
        expressions: Vec<TypedTemplateExpr>,
        last: Box<TypedTemplateExpr>,
    },
    If {
        token: Token,
        condition: TypedExpr,
        then: Box<TypedTemplateExpr>,
        otherwise: Box<TypedTemplateExpr>,
        result_type: ExprType,
    },
    Simple(TypedExpr),
}

impl TypedTemplateExpr {
    pub fn get_type(&self) -> ExprType {
        match self {
            TypedTemplateExpr::Block { last, .. } => last.get_type(),
            TypedTemplateExpr::If { result_type, .. } => result_type.clone(),
            TypedTemplateExpr::Simple(expr) => expr.get_type(),
        }
    }
}

impl Debug for TypedTemplateExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<type-checked template expr>")
    }
}

#[derive(Clone, PartialEq)]
pub enum TypedExpr {
    ConstantFloat(f64),
    ConstantInt(i32),
    ConstantBoolean(bool),
    ConstantString(String),
    Identifier {
        token: Token,
        reference: TypedImportableDecl,
        expr_type: ExprType,
    },

    UnaryOperator {
        operator: Token,
        expr: Box<TypedExpr>,
        result_type: ExprType,
    },
    BinaryOperator {
        left: Box<TypedExpr>,
        operator: Token,
        right: Box<TypedExpr>,
        result_type: ExprType,
    },
    FunctionCall {
        callee: Box<TypedExpr>,
        token: Token,
        args: Vec<TypedExpr>,
        result_type: ExprType,
    },
    Member {
        receiver: Box<TypedExpr>,
        name: Token,
        result_type: ExprType,
    },
    Index {
        receiver: Box<TypedExpr>,
        operator: Token,
        index: Box<TypedExpr>,
        result_type: ExprType,
    },
    BuiltinFunctionCall {
        name: Token,
        args: Vec<TypedExpr>,
        result_type: ExprType,
    },
    BuiltinType(ExprType),

    Object(Vec<(Token, TypedExpr)>),
    Array(Vec<TypedExpr>),

    Error,
}

impl TypedExpr {
    pub fn get_type(&self) -> ExprType {
        match self {
            TypedExpr::ConstantFloat(_) => ExprType::Float,
            TypedExpr::ConstantInt(_) => ExprType::Int,
            TypedExpr::ConstantBoolean(_) => ExprType::Boolean,
            TypedExpr::ConstantString(_) => ExprType::String,
            TypedExpr::Identifier { expr_type, .. } => expr_type.clone(),
            TypedExpr::UnaryOperator { result_type, .. } => result_type.clone(),
            TypedExpr::BinaryOperator { result_type, .. } => result_type.clone(),
            TypedExpr::FunctionCall { result_type, .. } => result_type.clone(),
            TypedExpr::Member { result_type, .. } => result_type.clone(),
            TypedExpr::Index { result_type, .. } => result_type.clone(),
            TypedExpr::BuiltinFunctionCall { result_type, .. } => result_type.clone(),
            TypedExpr::BuiltinType(_) => ExprType::Type,
            TypedExpr::Object(_) => ExprType::Object,
            TypedExpr::Array(_) => ExprType::Array,
            TypedExpr::Error => ExprType::Error,
        }
    }
}

impl Debug for TypedExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<type-checked expr>")
    }
}
