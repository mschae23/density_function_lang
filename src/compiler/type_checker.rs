use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::{Rc, Weak};
use crate::compiler::ast::simple::{Expr, TemplateExpr, VariableType};
use crate::compiler::lexer::{Token, TokenPos};
use crate::{Config, Decl};
use crate::compiler::ast::typed::{ExprType, ModuleDeclaration, TemplateDeclaration, TypedDecl, TypedExpr, TypedImportableDecl, TypedModuleDecl, TypedTemplateDecl, TypedTemplateExpr, TypedToken, TypedVariableDecl, VariableDeclaration};

pub mod hint;

use hint::*;

struct TypeEnvironment {
    templates: Vec<Rc<RefCell<TemplateDeclaration>>>,
    modules: HashMap<String, Rc<RefCell<ModuleDeclaration>>>,
    variables: HashMap<String, Rc<RefCell<VariableDeclaration>>>,

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

    /* pub fn contains(&self, name: &str) -> bool {
        self.modules.contains_key(name) || self.variables.contains_key(name)
        || self.templates.iter().any(|(template_name, _, _)| *template_name == *name)
    } */

    pub fn compare_types(&self, expected: &ExprType, found: &ExprType, allow_coerce: bool) -> bool {
        if allow_coerce {
            found.can_coerce_to(expected)
        } else {
            *found == *expected
        }
    }

    pub fn find_template(&self, name: &str, template_type: &TemplateType, allow_coerce: bool, recurse_to_parent: bool) -> Option<Option<Rc<RefCell<TemplateDeclaration>>>> {
        let mut matching_exact = Vec::new();
        let mut matching_coerce = Vec::new();

        for template in self.templates.iter().filter(|template| template.borrow().name.source() == name) {
            let template_borrow = template.borrow();
            let this = template_borrow.this.as_ref().map(|token| token.expr_type.clone());
            let args: Vec<ExprType> = template_borrow.args.iter().map(|arg| arg.expr_type.clone()).collect();
            let return_type = template_borrow.return_type.clone();

            if template_type.matches(this.as_ref(), &args, &return_type, false) {
                matching_exact.push(Rc::clone(template));
            } else if allow_coerce && template_type.matches(this.as_ref(), &args, &return_type, allow_coerce) {
                matching_coerce.push(Rc::clone(template));
            }
        }

        if matching_exact.len() == 1 {
            Some(Some(matching_exact.swap_remove(0)))
        } else if matching_exact.len() > 1 {
            Some(None)
        } else if matching_coerce.len() == 1 {
            Some(Some(matching_coerce.swap_remove(0)))
        } else if matching_coerce.len() > 1 {
            Some(None)
        } else if recurse_to_parent {
            self.parent.as_ref().and_then(|parent| parent.upgrade()).and_then(|parent| parent
                .borrow().find_template(name, template_type, allow_coerce, recurse_to_parent))
        } else {
            None
        }
    }

    pub fn find_module(&self, name: &str, recurse_to_parent: bool) -> Option<Rc<RefCell<ModuleDeclaration>>> {
        let found = self.modules.get(name).map(Rc::clone);

        if recurse_to_parent {
            found.or_else(|| self.parent.as_ref().and_then(|parent| parent.upgrade())
                .and_then(|parent| parent.borrow().find_module(name, recurse_to_parent)))
        } else {
            found
        }
    }

    pub fn find_variable(&self, name: &str, recurse_to_parent: bool) -> Option<Rc<RefCell<VariableDeclaration>>> {
        let found = self.variables.get(name).map(Rc::clone);

        if recurse_to_parent {
            found.or_else(|| self.parent.as_ref().and_then(|parent| parent.upgrade())
                .and_then(|parent| parent.borrow().find_variable(name, recurse_to_parent)))
        } else {
            found
        }
    }

    pub fn put_template(&mut self, name: Token, this: Option<TypedToken>, args: Vec<TypedToken>, return_type: ExprType) -> bool {
        let found = self.find_template(name.source(), &TemplateType {
            this: this.as_ref().map(|token| Box::new(token.expr_type.to_type_hint())),
            args: args.iter().map(|token| token.expr_type.to_type_hint()).collect::<Vec<TypeHint>>(),
            return_type: Box::new(return_type.to_type_hint()),
        }, false, false);

        if found.is_some() {
            false
        } else {
            self.templates.push(Rc::new(RefCell::new(TemplateDeclaration { name, this, args, return_type })));
            true
        }
    }

    pub fn put_variable(&mut self, name: Token, expr_type: ExprType, kind: VariableType) -> bool {
        match self.variables.entry(name.source().to_owned()) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(Rc::new(RefCell::new(VariableDeclaration { name, expr_type, kind })));
                true
            },
        }
    }

    pub fn put_module(&mut self, name: Token) -> bool {
        match self.modules.entry(name.source().to_owned()) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(Rc::new(RefCell::new(ModuleDeclaration { name })));
                true
            },
        }
    }

    pub fn add_child(&mut self, environment: Rc<RefCell<TypeEnvironment>>) {
        self.children.push(environment);
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
        for decl in &declarations {
            self.declare_declaration(decl);
        }

        let mut typed_decls = Vec::new();

        for decl in declarations {
            typed_decls.push(self.check_declaration(decl));
        }

        typed_decls
    }

    fn declare_declaration(&mut self, decl: &Decl) {
        match decl {
            Decl::Template { name, this, args, return_type, expr: _ } => {
                if !self.environment.borrow_mut().put_template(name.clone(), this.clone(), args.clone(), return_type.clone()) {
                    self.error_at(*name.start(), "Template with duplicate name and parameter list", false);
                }
            },
            Decl::Variable { name, expr_type, expr: _, kind } => {
                if !self.environment.borrow_mut().put_variable(name.clone(), expr_type.clone(), kind.clone()) {
                    self.error_at(*name.start(), "Variable with duplicate name", false);
                }
            },
            Decl::Module { name, statements: _ } => {
                if !self.environment.borrow_mut().put_module(name.clone()) {
                    self.error_at(*name.start(), "Module with duplicate name", false);
                }
            },
            Decl::Include { .. } => {},
            Decl::Import { .. } => {},
            Decl::Error => {},
        }
    }

    fn check_declaration(&mut self, decl: Decl) -> TypedDecl {
        match decl {
            Decl::Template { name, this, args, return_type, expr } => {
                let typed_expr = self.check_template_expr(expr, false, return_type.to_type_hint());
                let expr_type = typed_expr.get_type();

                if !expr_type.can_coerce_to(&return_type) {
                    self.error_at_with_context(*name.start(), "Mismatched types: template expression cannot be converted to the template's return type", vec![
                        ("Expected", &return_type.to_type_hint()),
                        ("Found", &expr_type.to_type_hint())
                    ], false);
                }

                let final_return_type = if return_type == ExprType::Any { expr_type } else { return_type };

                let template_decl = Rc::new(RefCell::new(TypedTemplateDecl {
                    name, this, args, return_type: final_return_type, expr: typed_expr,
                }));

                TypedDecl::Template(template_decl)
            },
            Decl::Variable { name, expr_type, expr, kind } => {
                let typed_expr = self.check_expr(expr, false, expr_type.to_type_hint());
                let actual_expr_type = typed_expr.get_type();

                if !actual_expr_type.can_coerce_to(&expr_type) {
                    self.error_at_with_context(*name.start(), "Mismatched types: variable expression cannot be converted to the declared type", vec![
                        ("Expected", &expr_type.to_type_hint()),
                        ("Found", &actual_expr_type.to_type_hint())
                    ], false);
                }

                let final_expr_type = if expr_type == ExprType::Any { actual_expr_type } else { expr_type };

                let variable = Rc::new(RefCell::new(TypedVariableDecl {
                    name,
                    expr_type: final_expr_type,
                    expr: typed_expr,
                    kind,
                }));

                TypedDecl::Variable(variable)
            },
            Decl::Module { name, statements: declarations } => {
                let current_environment = self.environment();
                let child_environment = TypeEnvironment::new_with_parent(Rc::clone(&current_environment));
                self.environment = Rc::clone(&child_environment);

                let typed_declarations = declarations.into_iter().map(|decl| self.check_declaration(decl)).collect();

                current_environment.borrow_mut().add_child(child_environment);
                self.environment = current_environment;

                let module = Rc::new(RefCell::new(TypedModuleDecl {
                    name,
                    declarations: typed_declarations,
                }));

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

    fn check_template_expr(&mut self, expr: TemplateExpr, static_expr: bool, type_hint: TypeHint) -> TypedTemplateExpr {
        match expr {
            TemplateExpr::Block { expressions, last } => {
                TypedTemplateExpr::Block {
                    expressions: expressions.into_iter()
                        .map(|expr| self.check_template_expr(expr, static_expr, TypeHint::Any)).collect(),
                    last: Box::new(self.check_template_expr(*last, static_expr, type_hint)),
                }
            },
            TemplateExpr::If { token, condition, then, otherwise } => {
                let typed_condition = self.check_expr(condition, true, TypeHint::Simple(vec![SimpleType::Boolean]));
                let typed_then = self.check_template_expr(*then, static_expr, type_hint.clone());
                let typed_otherwise = self.check_template_expr(*otherwise, static_expr, type_hint);

                if !typed_condition.get_type().can_coerce_to(&ExprType::Boolean) {
                    self.error_at_with_context(*token.start(), "Mismatched types: condition of 'if' expression must be of type 'boolean'", vec![
                        ("Expected", &TypeHint::Simple(vec![SimpleType::Boolean])),
                        ("Found", &typed_condition.get_type().to_type_hint())
                    ], false);
                }

                let then_type = typed_then.get_type();
                let otherwise_type = typed_otherwise.get_type();

                let result_type = if otherwise_type.can_coerce_to(&then_type) {
                    then_type
                } else if then_type.can_coerce_to(&otherwise_type) {
                    otherwise_type
                } else {
                    self.error_at_with_context(*token.start(), "Mismatched types: then and else branches of 'if' expression have different types", vec![
                        ("Then", &then_type.to_type_hint()),
                        ("Else", &otherwise_type.to_type_hint()),
                    ], false);
                    ExprType::Any
                };

                TypedTemplateExpr::If { token, condition: typed_condition,
                    then: Box::new(typed_then), otherwise: Box::new(typed_otherwise), result_type }
            },
            TemplateExpr::Simple(expr) => TypedTemplateExpr::Simple(self.check_expr(expr, static_expr, type_hint)),
        }
    }

    fn check_expr(&mut self, expr: Expr, static_expr: bool, type_hint: TypeHint) -> TypedExpr {
        match expr {
            Expr::ConstantFloat(value) => TypedExpr::ConstantFloat(value),
            Expr::ConstantInt(value) => TypedExpr::ConstantInt(value),
            Expr::ConstantBoolean(value) => TypedExpr::ConstantBoolean(value),
            Expr::ConstantString(value) => TypedExpr::ConstantString(value),
            // TODO implement type-checking for expressions
            Expr::Identifier(token) => {
                TypedExpr::Error
            },
            Expr::UnaryOperator { operator, expr } => {
                TypedExpr::Error
            },
            Expr::BinaryOperator { left, operator, right } => {
                TypedExpr::Error
            },
            Expr::FunctionCall { callee, token, args } => {
                TypedExpr::Error
            },
            Expr::Member { receiver, name } => {
                TypedExpr::Error
            },
            Expr::Index { receiver, operator, index } => {
                TypedExpr::Error
            },
            Expr::BuiltinFunctionCall { name, args } => {
                TypedExpr::Error
            },
            Expr::BuiltinType(ty) => TypedExpr::BuiltinType(ty),
            Expr::Object(fields) => {
                TypedExpr::Object(fields.into_iter().map(|(token, expr)|
                    (token, self.check_expr(expr, static_expr, TypeHint::Any)))
                    .collect())
            },
            Expr::Array(elements) => {
                TypedExpr::Array(elements.into_iter()
                    .map(|expr| self.check_expr(expr, static_expr, TypeHint::Any))
                    .collect())
            },
            Expr::Error => TypedExpr::Error,
        }
    }

    fn resolve_reference(&mut self, name: &Token, type_hint: TypeHint) -> Option<TypedImportableDecl> {
        match &type_hint {
            type_hint @ TypeHint::Template(template_type) => {
                match self.resolve_template(name, template_type) {
                    Some(Some(result)) => Some(TypedImportableDecl::Template(result)),
                    Some(None) => {
                        self.error_at_with_context(*name.start(), "Ambiguous reference to template", vec![
                            ("Expected", type_hint),
                        ], false);
                        None
                    },
                    None => {
                        self.error_at_with_context(*name.start(), "Template cannot be found", vec![
                            ("Expected", type_hint),
                        ], false);
                        None
                    },
                }
            },
            TypeHint::Module => {
                match self.resolve_module(name) {
                    Some(module) => Some(TypedImportableDecl::Module(module)),
                    None => {
                        self.error_at(*name.start(), "Module cannot be found", false);
                        None
                    }
                }
            },
            type_hint => {
                match self.resolve_variable(name) {
                    Some(variable) => {
                        if !type_hint.matches(&variable.borrow().expr_type, true) {
                            self.error_at_with_context(*name.start(), "Mismatched types", vec![
                                ("Expected", type_hint),
                                ("Found", &variable.borrow().expr_type.to_type_hint())
                            ], false);
                            None
                        } else {
                            Some(TypedImportableDecl::Variable(variable))
                        }
                    }
                    None => {
                        self.error_at(*name.start(), "Variable cannot be found", false);
                        None
                    }
                }
            },
        }
    }

    fn resolve_template(&mut self, name: &Token, template_type: &TemplateType) -> Option<Option<Rc<RefCell<TemplateDeclaration>>>> {
        self.environment.borrow().find_template(name.source(), template_type, true, true)
            .filter(|template| template.as_ref()
                .map(|template| template_type.return_type.matches(&template.borrow().return_type, true))
                .unwrap_or(true))
    }

    fn resolve_module(&self, name: &Token) -> Option<Rc<RefCell<ModuleDeclaration>>> {
        self.environment.borrow().find_module(name.source(), true)
    }

    fn resolve_variable(&self, name: &Token) -> Option<Rc<RefCell<VariableDeclaration>>> {
        self.environment.borrow().find_variable(name.source(), true)
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

    fn error_at_with_context(&mut self, pos: TokenPos, message: &str, context: Vec<(&str, &TypeHint)>, panic: bool) {
        if self.panic_mode {
            return;
        } else if panic {
            self.panic_mode = true;
        }

        eprintln!("[{}:{}:{}] Error: {}", self.path.to_string_lossy(), pos.line, pos.column, message);

        for (name, context_element) in context {
            eprintln!("    {}: {:?}", name, context_element);
        }

        self.had_error = true;
    }
}
