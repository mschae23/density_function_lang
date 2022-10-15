use std::fmt::{Debug, Formatter};
use crate::compiler::ast::typed::ExprType;

#[derive(Clone, PartialEq, Eq)]
pub enum SimpleType {
    Int, Float, Boolean, String,
    Object, Array,
    Type,
}

impl SimpleType {
    pub fn to_expr_type(&self) -> ExprType {
        match self {
            SimpleType::Int => ExprType::Int,
            SimpleType::Float => ExprType::Float,
            SimpleType::Boolean => ExprType::Boolean,
            SimpleType::String => ExprType::String,
            SimpleType::Object => ExprType::Object,
            SimpleType::Array => ExprType::Array,
            SimpleType::Type => ExprType::Type,
        }
    }
}

impl Debug for SimpleType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SimpleType::Int => write!(f, "int"),
            SimpleType::Float => write!(f, "float"),
            SimpleType::Boolean => write!(f, "boolean"),
            SimpleType::String => write!(f, "string"),
            SimpleType::Object => write!(f, "object"),
            SimpleType::Array => write!(f, "array"),
            SimpleType::Type => write!(f, "type"),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct TemplateType {
    pub this: Option<Box<TypeHint>>,
    pub args: Vec<TypeHint>,
    pub return_type: Box<TypeHint>,
}

impl TemplateType {
    pub fn matches(&self, other_this: Option<&ExprType>, other_args: &[ExprType], other_return_type: &ExprType, allow_coerce: bool) -> bool {
        self.this.is_some() == other_this.is_some()
            && self.this.as_ref().map(|hint| other_this.map(|ty| hint.matches(&*ty, allow_coerce))
            .unwrap_or(true)).unwrap_or(true)
            && self.args.len() == other_args.len()
            && self.args.iter().zip(other_args.iter())
            .map(|(hint, other_arg)| hint.matches(other_arg, allow_coerce)).all(|b| b)
            && self.return_type.matches(other_return_type, allow_coerce)
    }
}

impl Debug for TemplateType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}): {:?}", self.this.iter().map(|this| format!("this: {:?}", this))
            .chain(self.args.iter().map(|arg| format!("{:?}", arg)))
            .fold(String::new(), |acc, arg| format!("{}, {}", acc, arg)),
            &self.return_type)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeHint {
    Simple(SimpleType),
    Template(TemplateType),
    Module,
    Options(Vec<TypeHint>),
    Any,
}

impl TypeHint {
    pub fn matches(&self, other: &ExprType, allow_coerce: bool) -> bool {
        match self {
            TypeHint::Options(options) => {
                if options.is_empty() {
                    return allow_coerce;
                }

                for hint in options {
                    if hint.matches(other, allow_coerce) {
                        return true;
                    }
                }

                false
            },
            _ => match other {
                ExprType::Template { this: other_this, args: other_args, return_type: other_return_type } => match self {
                    TypeHint::Template(template) => {
                        template.matches(other_this.as_ref().map(|ty| &**ty), other_args, other_return_type, allow_coerce)
                    },
                    TypeHint::Any => allow_coerce,
                    _ => false,
                },
                ExprType::Module => *self == TypeHint::Module || (allow_coerce && *self == TypeHint::Any),
                ExprType::Any => *self == TypeHint::Any,
                ExprType::Error => false,
                expr_type => match self {
                    TypeHint::Simple(simple_type) => if allow_coerce {
                        simple_type.to_expr_type().can_coerce_to(expr_type)
                    } else {
                        simple_type.to_expr_type() == *expr_type
                    },
                    TypeHint::Any => allow_coerce,
                    _ => false,
                },
            },
        }
    }
}

impl TypeHint {
    pub fn from_expr_type(expr_type: &ExprType) -> Self {
        match expr_type {
            ExprType::Int => TypeHint::Simple(SimpleType::Int),
            ExprType::Float => TypeHint::Simple(SimpleType::Float),
            ExprType::Boolean => TypeHint::Simple(SimpleType::Boolean),
            ExprType::String => TypeHint::Simple(SimpleType::String),
            ExprType::Object => TypeHint::Simple(SimpleType::Object),
            ExprType::Array => TypeHint::Simple(SimpleType::Array),
            ExprType::Type => TypeHint::Simple(SimpleType::Type),
            ExprType::Template { this, args, return_type } =>
                TypeHint::Template(TemplateType {
                    this: this.as_ref().map(|this| Box::new(this.to_type_hint())),
                    args: args.iter().map(|arg| arg.to_type_hint()).collect(),
                    return_type: Box::new(return_type.to_type_hint()),
                }),
            ExprType::Module => TypeHint::Module,
            ExprType::Any => TypeHint::Any,
            ExprType::Error => TypeHint::Any,
        }
    }
}

impl Debug for TypeHint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeHint::Simple(simple) => write!(f, "{:?}", simple),
            TypeHint::Template(template) => write!(f, "{:?}", template),
            TypeHint::Module => write!(f, "module"),
            TypeHint::Options(options) => {
                if options.is_empty() {
                    return write!(f, "_");
                } else {
                    if options.len() > 1 {
                        write!(f, "(")?;
                    }

                    write!(f, "{}", options.iter().map(|hint| format!("{:?}", hint))
                        .fold(String::new(), |acc, hint| format!("{} | {}", acc, hint)))?;

                    if options.len() > 1 {
                        write!(f, ")")?;
                    }

                    Ok(())
                }
            },
            TypeHint::Any => write!(f, "_"),
        }
    }
}
