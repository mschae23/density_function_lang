use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::simple::{Expr, TemplateExpr};
use crate::compiler::lexer::TokenPos;
use crate::{Config, Decl};
use crate::compiler::ast::typed::{ExprType, TypedDecl, TypedExpr, TypedImportableDecl, TypedModuleDecl, TypedTemplateDecl, TypedTemplateExpr, TypedToken, TypedVariableDecl};

struct TypeEnvironment {
    templates: Vec<(String, Rc<RefCell<TypedTemplateDecl>>, ExprType)>,
    modules: HashMap<String, (Rc<RefCell<TypedModuleDecl>>, ExprType)>,
    variables: HashMap<String, (Rc<RefCell<TypedVariableDecl>>, ExprType)>,
    parent: Option<Rc<RefCell<TypeEnvironment>>>,
}

impl TypeEnvironment {
    pub fn new_global() -> Rc<RefCell<TypeEnvironment>> {
        Rc::new(RefCell::new(TypeEnvironment {
            templates: Vec::new(),
            modules: HashMap::new(),
            variables: HashMap::new(),
            parent: None,
        }))
    }

    pub fn new_with_parent(parent: Rc<RefCell<TypeEnvironment>>) -> Rc<RefCell<TypeEnvironment>> {
        Rc::new(RefCell::new(TypeEnvironment {
            templates: Vec::new(),
            modules: HashMap::new(),
            variables: HashMap::new(),
            parent: Some(parent),
        }))
    }

    pub fn contains(&self, name: &str, recurse_to_parent: bool) -> bool {
        // TODO implement lookup in parent
        self.modules.contains_key(name) || self.variables.contains_key(name)
        || self.templates.iter().any(|(template_name, _, _)| *template_name == *name)
    }

    pub fn find_template(&self, name: &str, this: Option<ExprType>, args: Vec<ExprType>) -> Option<(Rc<RefCell<TypedTemplateDecl>>, ExprType)> {
        self.find_template_impl(name, this.as_ref(), &args, true)
    }

    fn find_template_impl(&self, name: &str, this: Option<&ExprType>, args: &[ExprType], recurse_to_parent: bool) -> Option<(Rc<RefCell<TypedTemplateDecl>>, ExprType)> {
        // TODO lookup in parent
        self.templates.iter().find(|(template_name, template, _)| *template_name == *name
            && template.borrow().this.is_some() == this.is_some()
            && template.borrow().this.and_then(|token| this.map(|expr_type| token.expr_type == *expr_type)).unwrap_or(true)
            && template.borrow().args.len() == args.len()
            && template.borrow().args.iter().map(|arg| arg.expr_type.clone())
            .zip(args.iter()).map(|(left, right)| left == right.clone()).all(|b| b))
            .map(|(name, decl, expr_type)| (Rc::clone(decl), expr_type.clone()))
    }

    pub fn put_template(&mut self, name: String, decl: Rc<RefCell<TypedTemplateDecl>>, expr_type: ExprType) -> bool {
        if self.find_template_impl(&name,
            decl.borrow().this.as_ref().map(|token| &token.expr_type),
        &decl.borrow().args.iter().map(|token| token.expr_type.clone()).collect::<Vec<ExprType>>(), false).is_some() {
            false
        } else {
            self.templates.push((name, decl, expr_type));
            true
        }
    }
}

pub struct TypeChecker {
    environment: Rc<RefCell<TypeEnvironment>>,

    path: PathBuf, target_dir: PathBuf,

    had_error: bool, panic_mode: bool,
    config: Rc<Config>,
}

impl TypeChecker {
    pub fn new(path: PathBuf, target_dir: PathBuf, config: Rc<Config>) -> TypeChecker {
        TypeChecker {
            environment: TypeEnvironment::new_global(),
            path, target_dir,
            had_error: false, panic_mode: false,
            config,
        }
    }

    pub fn check_types(&mut self, declarations: Vec<Decl>) -> Vec<TypedDecl> {
        let mut typed_decls = Vec::new();

        for decl in declarations {
            typed_decls.push(self.check_declaration(decl));
        }

        typed_decls
    }

    fn check_declaration(&mut self, decl: Decl) -> TypedDecl {
        match decl {
            Decl::Template { name, this, args, expr } => {
                if self.environment.borrow().contains(name.source(), false) {
                    self.error_at(*name.start(), "Template with duplicate name", false);
                }

                TypedDecl::Template(Rc::new(RefCell::new(TypedTemplateDecl {
                    name,
                    this: this.map(|token| TypedToken { token, expr_type: ExprType::Any }),
                    args: args.into_iter().map(|token| TypedToken { token, expr_type: ExprType::Any }).collect(),
                    expr: self.check_template_expr(expr),
                })))
            },
            Decl::Variable { name, expr, kind } => {
                TypedDecl::Variable(Rc::new(RefCell::new(TypedVariableDecl {
                    name,
                    expr: self.check_expr(expr),
                    kind,
                })))
            },
            Decl::Module { name, statements: declarations } => {
                TypedDecl::Module(Rc::new(RefCell::new(TypedModuleDecl {
                    name,
                    declarations: declarations.into_iter().map(|decl| self.check_declaration(decl)).collect(),
                }))) 
            }
            Decl::Include { path, declarations } => {
                self.error_at(*path.start(), "Include declarations are not supported yet", false);
                TypedDecl::Error
            },
            Decl::Import { path, selector } => {
                self.error_at(path.first()
                    .map(|token| *token.start())
                    .unwrap_or(TokenPos { line: 0, column: 0 }), "Import declarations are not supported yet", false);
                TypedDecl::Error
            },
            Decl::Error => TypedDecl::Error,
        }
    }

    fn check_template_expr(&mut self, expr: TemplateExpr) -> TypedTemplateExpr {
        match expr {
            TemplateExpr::Block { expressions, last } => {
                TypedTemplateExpr::Block {
                    expressions: expressions.into_iter().map(|expr| self.check_template_expr(expr)).collect(),
                    last: Box::new(self.check_template_expr(*last)),
                }
            },
            TemplateExpr::If { token, condition, then, otherwise } => {
                let typed_condition = self.check_expr(condition);
                let typed_then = self.check_template_expr(*then);
                let typed_otherwise = self.check_template_expr(*otherwise);

                if typed_condition.get_type() != ExprType::Boolean {
                    self.error_at(*token.start(), "Condition of 'if' expression must be of type 'boolean'", false);
                }

                let result_type = if typed_then.get_type() != typed_otherwise.get_type() {
                    self.error_at(*token.start(), "then and else branches of 'if' expression have different types", false);
                    ExprType::Any
                } else {
                    typed_then.get_type()
                };

                TypedTemplateExpr::If { token, condition: typed_condition,
                    then: Box::new(typed_then), otherwise: Box::new(typed_otherwise), result_type }
            },
            TemplateExpr::Simple(expr) => TypedTemplateExpr::Simple(self.check_expr(expr)),
        }
    }

    fn check_expr(&mut self, expr: Expr) -> TypedExpr {
        match expr {
            Expr::ConstantFloat(value) => TypedExpr::ConstantFloat(value),
            Expr::ConstantInt(value) => TypedExpr::ConstantInt(value),
            Expr::ConstantBoolean(value) => TypedExpr::ConstantBoolean(value),
            Expr::ConstantString(value) => TypedExpr::ConstantString(value),
            // TODO implement type-checking for expressions
            Expr::Identifier(token) => {},
            Expr::UnaryOperator { operator, expr } => {},
            Expr::BinaryOperator { left, operator, right } => {},
            Expr::FunctionCall { callee, token, args } => {},
            Expr::Member { receiver, name } => {},
            Expr::Index { receiver, operator, index } => {},
            Expr::BuiltinFunctionCall { name, args } => {},
            Expr::BuiltinType(ty) => TypedExpr::BuiltinType(ty, ExprType::Type),
            Expr::Object(fields) => {},
            Expr::Array(elements) => {},
            Expr::Error => TypedExpr::Error,
        }
    }

    // Error handling

    pub fn had_error(&self) -> bool {
        self.had_error
    }

    fn error_at(&mut self, pos: TokenPos, message: &str, panic: bool) {
        self.error_at_with_context(pos, message, Vec::new(), panic)
    }

    fn error_at_with_context(&mut self, pos: TokenPos, message: &str, context: Vec<(&str, Expr)>, panic: bool) {
        if self.panic_mode {
            return;
        } else if panic {
            self.panic_mode = true;
        }

        eprintln!("[{}:{}:{}] Error: {}", self.path.to_string_lossy(), pos.line, pos.column, message);

        for (name, context_element) in &context {
            eprintln!("    {}: {:?}", *name, context_element);
        }

        self.had_error = true;
    }
}
