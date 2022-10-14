use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::{Rc, Weak};
use crate::compiler::ast::simple::{Expr, TemplateExpr, VariableType};
use crate::compiler::lexer::{Token, TokenPos};
use crate::{Config, Decl};
use crate::compiler::ast::typed::{ExprType, TypedDecl, TypedExpr, TypedModuleDecl, TypedTemplateDecl, TypedTemplateExpr, TypedToken, TypedVariableDecl};

struct TypeEnvironment {
    templates: Vec<(String, Rc<RefCell<TypedTemplateDecl>>, ExprType)>,
    modules: HashMap<String, Rc<RefCell<TypedModuleDecl>>>,
    variables: HashMap<String, (Rc<RefCell<TypedVariableDecl>>, ExprType)>,

    parent: Option<Weak<RefCell<TypeEnvironment>>>,
    children: Vec<Rc<RefCell<TypeEnvironment>>>,
}

impl TypeEnvironment {
    pub fn new_global() -> Rc<RefCell<TypeEnvironment>> {
        Rc::new(RefCell::new(TypeEnvironment {
            templates: Vec::new(),
            modules: HashMap::new(),
            variables: HashMap::new(),

            parent: None, children: Vec::new(),
        }))
    }

    pub fn new_with_parent(parent: Rc<RefCell<TypeEnvironment>>) -> Rc<RefCell<TypeEnvironment>> {
        Rc::new(RefCell::new(TypeEnvironment {
            templates: Vec::new(),
            modules: HashMap::new(),
            variables: HashMap::new(),

            parent: Some(Rc::downgrade(&parent)), children: Vec::new(),
        }))
    }

    pub fn contains(&self, name: &str) -> bool {
        self.modules.contains_key(name) || self.variables.contains_key(name)
        || self.templates.iter().any(|(template_name, _, _)| *template_name == *name)
    }

    pub fn find_template(&self, name: &str, this: Option<&ExprType>, args: &[ExprType], _recurse_to_parent: bool) -> Option<(Rc<RefCell<TypedTemplateDecl>>, ExprType)> {
        // TODO implement lookup in parent
        self.templates.iter().find(|(template_name, template, _)| *template_name == *name
            && template.borrow().this.is_some() == this.is_some()
            && template.borrow().this.and_then(|token| this.map(|expr_type| token.expr_type == *expr_type)).unwrap_or(true)
            && template.borrow().args.len() == args.len()
            && template.borrow().args.iter().map(|arg| arg.expr_type.clone())
            .zip(args.iter()).map(|(left, right)| left == right.clone()).all(|b| b))
            .map(|(name, decl, expr_type)| (Rc::clone(decl), expr_type.clone()))
    }

    pub fn put_template(&mut self, name: String, decl: Rc<RefCell<TypedTemplateDecl>>, expr_type: ExprType) -> bool {
        if self.find_template(&name,
            decl.borrow().this.as_ref().map(|token| &token.expr_type),
        &decl.borrow().args.iter().map(|token| token.expr_type.clone()).collect::<Vec<ExprType>>(), false).is_some() {
            false
        } else {
            self.templates.push((name, decl, expr_type));
            true
        }
    }

    pub fn put_variable(&mut self, name: String, decl: Rc<RefCell<TypedVariableDecl>>, expr_type: ExprType) -> bool {
        match self.variables.entry(name) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert((decl, expr_type));
                true
            },
        }
    }

    pub fn put_module(&mut self, name: String, decl: Rc<RefCell<TypedModuleDecl>>) -> bool {
        match self.modules.entry(name) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(decl);
                true
            },
        }
    }

    pub fn add_child(&mut self, environment: Rc<RefCell<TypeEnvironment>>) {
        self.children.push(environment);
    }
}

#[derive(Clone, PartialEq, Debug)]
struct TemplateDeclaration {
    pub name: Token,
    pub this: Option<TypedToken>,
    pub args: Vec<TypedToken>,
    pub return_type: ExprType,
}

#[derive(Clone, PartialEq, Debug)]
struct ModuleDeclaration {
    pub name: Token,
}

#[derive(Clone, PartialEq, Debug)]
struct VariableDeclaration {
    pub name: Token,
    pub kind: VariableType,
    pub expr_type: ExprType,
}

struct TypeDeclarationEnvironment {
    templates: Vec<TemplateDeclaration>,
    modules: HashMap<String, ModuleDeclaration>,
    variables: HashMap<String, VariableDeclaration>,
}

impl TypeDeclarationEnvironment {
    pub fn new() -> TypeDeclarationEnvironment {
        TypeDeclarationEnvironment {
            templates: Vec::new(),
            modules: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn find_template(&self, name: &str, this: Option<&ExprType>, args: &[ExprType]) -> Option<&TemplateDeclaration> {
        self.templates.iter().find(|template| *template.name.source() == *name
            && template.this.is_some() == this.is_some()
            && template.this.and_then(|token| this.map(|expr_type| token.expr_type == *expr_type)).unwrap_or(true)
            && template.args.len() == args.len()
            && template.args.iter().map(|arg| arg.expr_type.clone())
            .zip(args.iter()).map(|(left, right)| left == right.clone()).all(|b| b))
    }

    pub fn find_module(&self, name: &str) -> Option<&ModuleDeclaration> {
        self.modules.get(name)
    }

    pub fn find_variable(&self, name: &str) -> Option<&VariableDeclaration> {
        self.variables.get(name)
    }

    pub fn put_template(&mut self, name: Token, this: Option<TypedToken>, args: Vec<TypedToken>, return_type: ExprType) -> bool {
        if self.find_template(name.source(),
            this.as_ref().map(|token| &token.expr_type),
            &args.iter().map(|token| token.expr_type.clone()).collect::<Vec<ExprType>>()).is_some() {
            false
        } else {
            self.templates.push(TemplateDeclaration { name, this, args, return_type });
            true
        }
    }

    pub fn put_module(&mut self, name: Token) -> bool {
        match self.modules.entry(name.source().to_owned()) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(ModuleDeclaration { name });
                true
            },
        }
    }

    pub fn put_variable(&mut self, name: Token, kind: VariableType, expr_type: ExprType) -> bool {
        match self.variables.entry(name.source().to_owned()) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(VariableDeclaration { name, kind, expr_type });
                true
            },
        }
    }
}

pub struct TypeChecker {
    global_environment: Rc<RefCell<TypeEnvironment>>,
    environment: Rc<RefCell<TypeEnvironment>>,

    path: PathBuf, target_dir: PathBuf,

    had_error: bool, panic_mode: bool,
    config: Rc<Config>,
}

impl TypeChecker {
    pub fn new(path: PathBuf, target_dir: PathBuf, config: Rc<Config>) -> TypeChecker {
        let environment = TypeEnvironment::new_global();

        TypeChecker {
            global_environment: Rc::clone(&environment), environment,
            path, target_dir,
            had_error: false, panic_mode: false,
            config,
        }
    }

    pub fn check_types(&mut self, declarations: Vec<Decl>) -> Vec<TypedDecl> {
        let mut environment = TypeDeclarationEnvironment::new();

        for decl in &declarations {
            self.declare_declaration(decl, &mut environment);
        }

        let mut typed_decls = Vec::new();

        for decl in declarations {
            typed_decls.push(self.check_declaration(decl, &environment));
        }

        typed_decls
    }

    fn declare_declaration(&mut self, decl: &Decl, environment: &mut TypeDeclarationEnvironment) {
        match decl {
            Decl::Template { name, this, args, return_type, expr: _ } => {
                if !environment.put_template(name.clone(), this.clone(), args.clone(), return_type.clone()) {
                    self.error_at(*name.start(), "Template with duplicate name and parameter list", false);
                }
            },
            Decl::Variable { name, expr_type, expr: _, kind } => {
                if !environment.put_variable(name.clone(), kind.clone(), expr_type.clone()) {
                    self.error_at(*name.start(), "Variable with duplicate name", false);
                }
            },
            Decl::Module { name, statements: _ } => {
                if !environment.put_module(name.clone()) {
                    self.error_at(*name.start(), "Module with duplicate name", false);
                }
            },
            Decl::Include { .. } => {},
            Decl::Import { .. } => {},
            Decl::Error => {},
        }
    }

    fn check_declaration(&mut self, decl: Decl, environment: &TypeDeclarationEnvironment) -> TypedDecl {
        match decl {
            Decl::Template { name, this, args, return_type, expr } => {
                let typed_expr = self.check_template_expr(expr, vec![return_type], environment);
                let expr_type = typed_expr.get_type();

                if !expr_type.can_coerce_to(return_type.clone()) {
                    self.error_at_with_context(*name.start(), "Template expression cannot be converted to the template's return type", vec![
                        ("Expected", Expr::BuiltinType(return_type.clone())),
                        ("Found", Expr::BuiltinType(expr_type.clone()))
                    ], false);
                }

                let final_return_type = if return_type == ExprType::Any { expr_type } else { return_type };

                let template_decl = Rc::new(RefCell::new(TypedTemplateDecl {
                    name, this, args, return_type: final_return_type.clone(), expr: typed_expr,
                }));

                self.environment.borrow_mut().put_template(name.source().to_owned(), Rc::clone(&template_decl), final_return_type);
                TypedDecl::Template(template_decl)
            },
            Decl::Variable { name, expr_type, expr, kind } => {
                let typed_expr = self.check_expr(expr, vec![expr_type.clone()], environment);
                let actual_expr_type = typed_expr.get_type();

                if !actual_expr_type.can_coerce_to(expr_type.clone()) {
                    self.error_at_with_context(*name.start(), "Variable expression cannot be converted to the declared type", vec![
                        ("Expected", Expr::BuiltinType(expr_type.clone())),
                        ("Found", Expr::BuiltinType(actual_expr_type.clone()))
                    ], false);
                }

                let final_expr_type = if expr_type == ExprType::Any { actual_expr_type } else { expr_type };

                let variable = Rc::new(RefCell::new(TypedVariableDecl {
                    name,
                    expr_type: final_expr_type.clone(),
                    expr: typed_expr,
                    kind,
                }));

                self.environment.borrow_mut().put_variable(name.source().to_owned(), Rc::clone(&variable), final_expr_type);
                TypedDecl::Variable(variable)
            },
            Decl::Module { name, statements: declarations } => {
                let current_environment = self.environment();
                let child_environment = TypeEnvironment::new_with_parent(Rc::clone(&current_environment));
                self.environment = Rc::clone(&child_environment);

                let typed_declarations = declarations.into_iter().map(|decl| self.check_declaration(decl, environment)).collect();

                current_environment.borrow_mut().add_child(child_environment);
                self.environment = current_environment;

                let module = Rc::new(RefCell::new(TypedModuleDecl {
                    name,
                    declarations: typed_declarations,
                }));

                self.environment.borrow_mut().put_module(name.source().to_owned(), Rc::clone(&module));
                TypedDecl::Module(module)
            }
            Decl::Include { path, declarations: _ } => {
                self.error_at(*path.start(), "Include declarations are not supported yet", false);
                TypedDecl::Error
            },
            Decl::Import { path, selector: _ } => {
                self.error_at(path.first()
                    .map(|token| *token.start())
                    .unwrap_or(TokenPos { line: 0, column: 0 }), "Import declarations are not supported yet", false);
                TypedDecl::Error
            },
            Decl::Error => TypedDecl::Error,
        }
    }

    fn check_template_expr(&mut self, expr: TemplateExpr, type_hints: Vec<ExprType>, _environment: &TypeDeclarationEnvironment) -> TypedTemplateExpr {
        match expr {
            TemplateExpr::Block { expressions, last } => {
                TypedTemplateExpr::Block {
                    expressions: expressions.into_iter()
                        .map(|expr| self.check_template_expr(expr, vec![ExprType::Any], _environment)).collect(),
                    last: Box::new(self.check_template_expr(*last, type_hints, _environment)),
                }
            },
            TemplateExpr::If { token, condition, then, otherwise } => {
                let typed_condition = self.check_expr(condition, vec![ExprType::Boolean], _environment);
                let typed_then = self.check_template_expr(*then, type_hints.clone(), _environment);
                let typed_otherwise = self.check_template_expr(*otherwise, type_hints, _environment);

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
            TemplateExpr::Simple(expr) => TypedTemplateExpr::Simple(self.check_expr(expr, type_hints, _environment)),
        }
    }

    fn check_expr(&mut self, expr: Expr, _type_hints: Vec<ExprType>, _environment: &TypeDeclarationEnvironment) -> TypedExpr {
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
            Expr::BuiltinType(ty) => TypedExpr::BuiltinType(ty),
            Expr::Object(fields) => {},
            Expr::Array(elements) => {},
            Expr::Error => TypedExpr::Error,
        }
    }

    fn environment(&self) -> Rc<RefCell<TypeEnvironment>> {
        Rc::clone(&self.environment)
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
