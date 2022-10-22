use std::cmp::Ordering;
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
    pub fn new_any(this: bool, arg_count: usize, return_type: TypeHint) -> TemplateType {
        TemplateType {
            this: if this { Some(Box::new(TypeHint::Any)) } else { None },
            args: std::iter::once(TypeHint::Any).cycle().take(arg_count).collect(),
            return_type: Box::new(return_type)
        }
    }

    pub fn can_coerce_from(&self, other_this: Option<&ExprType>, other_args: &[ExprType], other_return_type: &ExprType, allow_coerce: bool) -> bool {
        self.this.is_some() == other_this.is_some()
            && self.this.as_ref().map(|this| other_this.map(|ty| this.can_coerce_to(&*ty, allow_coerce))
            .unwrap_or(true)).unwrap_or(true)
            && self.args.len() == other_args.len()
            && self.args.iter().zip(other_args.iter())
            .map(|(arg, other_arg)| arg.can_coerce_to(other_arg, allow_coerce)).all(|b| b)
            && self.return_type.can_coerce_from(other_return_type, allow_coerce)
    }

    pub fn can_coerce_to(&self, other_this: Option<&ExprType>, other_args: &[ExprType], other_return_type: &ExprType, allow_coerce: bool) -> bool {
        self.this.is_some() == other_this.is_some()
            && self.this.as_ref().map(|this| other_this.map(|ty| this.can_coerce_from(&*ty, allow_coerce))
            .unwrap_or(true)).unwrap_or(true)
            && self.args.len() == other_args.len()
            && self.args.iter().zip(other_args.iter())
            .map(|(arg, other_arg)| arg.can_coerce_from(other_arg, allow_coerce)).all(|b| b)
            && self.return_type.can_coerce_to(other_return_type, allow_coerce)
    }

    pub fn merge(templates: &[TemplateType], this: bool, arg_count: usize, return_type_hint: TypeHint) -> TemplateType {
        // All templates can have different types for this and arguments, but this.is_some() and args.len() must be the same

        if templates.is_empty() {
            TemplateType::new_any(this, arg_count, return_type_hint)
        } else {
            let has_this = templates[0].this.is_some();
            let mut this = Vec::new();

            if has_this {
                for template in templates {
                    match &template.this {
                        None => break,
                        Some(this_type) => {
                            if !this.contains(&**this_type) {
                                this.push((&**this_type).clone());
                            }
                        }
                    }
                }
            }

            let this = match this.len().cmp(&1) {
                Ordering::Less => TypeHint::Any,
                Ordering::Equal => this.swap_remove(0),
                Ordering::Greater => TypeHint::Options(this),
            };

            let arg_count = templates[0].args.len();
            let mut args = Vec::new();

            for i in 0..arg_count {
                let mut types = Vec::new();

                for template in templates {
                    let hint = &template.args[i];

                    if !types.contains(hint) {
                        types.push(hint.clone());
                    }
                }

                args.push(match types.len().cmp(&1) {
                    Ordering::Less => TypeHint::Any,
                    Ordering::Equal => types.swap_remove(0),
                    Ordering::Greater => TypeHint::Options(types),
                });
            }

            let return_type = {
                let mut types = vec![return_type_hint];

                for template in templates {
                    let hint = &*template.return_type;

                    if !types.contains(hint) {
                        types.push(hint.clone());
                    }
                }

                Box::new(match types.len().cmp(&1) {
                    Ordering::Less => TypeHint::Any,
                    Ordering::Equal => types.swap_remove(0),
                    Ordering::Greater => TypeHint::Options(types),
                })
            };

            TemplateType {
                this: if has_this { Some(Box::new(this)) } else { None },
                args,
                return_type,
            }
        }
    }
}

impl Debug for TemplateType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}): {:?}", self.this.iter().map(|this| format!("this: {:?}", this))
            .chain(self.args.iter().map(|arg| format!("{:?}", arg)))
            .collect::<Vec<String>>().join(", "),
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
    Error,
}

impl TypeHint {
    pub fn can_coerce_from(&self, other: &ExprType, allow_coerce: bool) -> bool {
        match self {
            TypeHint::Options(options) => {
                if options.is_empty() {
                    return TypeHint::Any.can_coerce_from(other, allow_coerce);
                }

                for hint in options {
                    if hint.can_coerce_from(other, allow_coerce) {
                        return true;
                    }
                }

                false
            },
            TypeHint::Error => *other == ExprType::Error,
            TypeHint::Any => *other == ExprType::Any || allow_coerce,
            _ => match other {
                ExprType::Template { this: other_this, args: other_args, return_type: other_return_type } => match self {
                    TypeHint::Template(template) => {
                        template.can_coerce_from(other_this.as_ref().map(|ty| &**ty), other_args, other_return_type, allow_coerce)
                    },
                    _ => false,
                },
                ExprType::Module => *self == TypeHint::Module,
                ExprType::Any => false, // Would need self == Any, but that was already checked and is false
                ExprType::Error => true,
                expr_type => match self {
                    TypeHint::Simple(simple_type) => {
                        if allow_coerce {
                            expr_type.can_coerce_to(&simple_type.to_expr_type())
                        } else {
                            simple_type.to_expr_type() == *expr_type
                        }
                    },
                    _ => false,
                },
            },
        }
    }

    pub fn can_coerce_to(&self, other: &ExprType, allow_coerce: bool) -> bool {
        match self {
            TypeHint::Options(options) => {
                if options.is_empty() {
                    return TypeHint::Any.can_coerce_to(other, allow_coerce);
                }

                for self_type in options {
                    if self_type.can_coerce_to(other, allow_coerce) {
                        return true;
                    }
                }

                false
            },
            TypeHint::Error => true,
            TypeHint::Any => *other == ExprType::Any,
            _ => match other {
                ExprType::Template { this: other_this, args: other_args, return_type: other_return_type } => match self {
                    TypeHint::Template(template) => {
                        template.can_coerce_to(other_this.as_ref().map(|ty| &**ty), other_args, other_return_type, allow_coerce)
                    },
                    _ => false,
                },
                ExprType::Module => *self == TypeHint::Module,
                ExprType::Any => allow_coerce,
                ExprType::Error => false,
                expr_type => match self {
                    TypeHint::Simple(simple_type) => {
                        if allow_coerce {
                            simple_type.to_expr_type().can_coerce_to(expr_type)
                        } else {
                            simple_type.to_expr_type() == *expr_type
                        }
                    },
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
            ExprType::Error => TypeHint::Error,
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
                        .collect::<Vec<String>>().join(" | "))?;

                    if options.len() > 1 {
                        write!(f, ")")?;
                    }

                    Ok(())
                }
            },
            TypeHint::Any => write!(f, "_"),
            TypeHint::Error => write!(f, "error"),
        }
    }
}
