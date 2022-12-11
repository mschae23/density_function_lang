use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::{Rc, Weak};
use crate::compiler::ast::simple::{Expr, TemplateExpr};
use crate::compiler::lexer::{Token, TokenPos, TokenType};
use crate::{Config, Decl};
use crate::compiler::ast::typed::{ExprType, ModuleDeclaration, TemplateDeclaration, TypedDecl, TypedExpr, TypedImportableDecl, TypedModuleDecl, TypedTemplateDecl, TypedTemplateExpr, TypedVariableDecl, VariableDeclaration};

pub mod hint;

use hint::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RecursionToParent {
    Always,
    Once,
    Never,
}

impl RecursionToParent {
    pub fn should_recurse(&self) -> bool {
        match self {
            RecursionToParent::Always | RecursionToParent::Once => true,
            RecursionToParent::Never => false,
        }
    }

    pub fn decrement(self) -> Self {
        match self {
            RecursionToParent::Always => RecursionToParent::Always,
            RecursionToParent::Once => RecursionToParent::Never,
            RecursionToParent::Never => RecursionToParent::Never,
        }
    }
}

#[derive(Debug)]
struct TypeEnvironment {
    templates: Vec<Rc<RefCell<TemplateDeclaration>>>,
    modules: HashMap<String, Rc<RefCell<ModuleDeclaration>>>,
    variables: HashMap<String, Rc<RefCell<VariableDeclaration>>>,

    parent: Option<Weak<RefCell<TypeEnvironment>>>,
    children: HashMap<String, Rc<RefCell<TypeEnvironment>>>,
    imports: Option<Rc<RefCell<TypeEnvironment>>>,
}

impl TypeEnvironment {
    pub fn new_global() -> Rc<RefCell<TypeEnvironment>> {
        Rc::new(RefCell::new(TypeEnvironment {
            templates: Vec::new(),
            modules: HashMap::new(),
            variables: HashMap::new(),

            parent: None, children: HashMap::new(),
            imports: None,
        }))
    }

    pub fn new_with_parent(parent: Rc<RefCell<TypeEnvironment>>) -> Rc<RefCell<TypeEnvironment>> {
        Rc::new(RefCell::new(TypeEnvironment {
            templates: Vec::new(),
            modules: HashMap::new(),
            variables: HashMap::new(),

            parent: Some(Rc::downgrade(&parent)), children: HashMap::new(),
            imports: None,
        }))
    }

    pub fn get_or_create_import_environment(this: Rc<RefCell<TypeEnvironment>>) -> Rc<RefCell<TypeEnvironment>> {
        let mut this_borrow = this.borrow_mut();
        let imports_field = &mut this_borrow.imports;

        match imports_field {
            Some(imports) => Rc::clone(imports),
            None => {
                let imports = Self::new_with_parent(Rc::clone(&this));
                *imports_field = Some(Rc::clone(&imports));
                imports
            },
        }
    }

    #[allow(unused)]
    pub fn compare_types(&self, expected: &ExprType, found: &ExprType, allow_coerce: bool) -> bool {
        if allow_coerce {
            found.can_coerce_to(expected)
        } else {
            *found == *expected
        }
    }

    pub fn find_template_options(&self, name: &str, this: bool, arg_count: usize, recursion: RecursionToParent) -> Vec<Rc<RefCell<TemplateDeclaration>>> {
        let found: Vec<Rc<RefCell<TemplateDeclaration>>> = self.templates.iter().filter(|&template| {
            let template_borrow = template.borrow();

            template_borrow.name.source() == name
            && template_borrow.this.is_some() == this
            && template_borrow.args.len() == arg_count
        }).map(Rc::clone).collect();

        if recursion.should_recurse() && found.is_empty() {
            self.parent.as_ref().and_then(|parent| parent.upgrade()).into_iter().flat_map(|parent| parent
                .borrow().find_template_options(name, this, arg_count, recursion.decrement()).into_iter()).collect()
        } else {
            found
        }
    }

    pub fn find_template(&self, name: &str, template_type: &TemplateType, allow_coerce: bool, recursion: RecursionToParent) -> Vec<Rc<RefCell<TemplateDeclaration>>> {
        let mut matching_exact = Vec::new();
        let mut matching_coerce = Vec::new();
        let mut matching_name = 0;

        for template in self.templates.iter().filter(|template| template.borrow().name.source() == name) {
            let template_borrow = template.borrow();
            let this = template_borrow.this.as_ref().map(|token| token.expr_type.clone());
            let args: Vec<ExprType> = template_borrow.args.iter().map(|arg| arg.expr_type.clone()).collect();
            let return_type = template_borrow.return_type.clone();

            matching_name += 1;

            if template_type.can_coerce_from(this.as_ref(), &args, &return_type, false) {
                matching_exact.push(Rc::clone(template));
            } else if allow_coerce && template_type.can_coerce_from(this.as_ref(), &args, &return_type, allow_coerce) {
                matching_coerce.push(Rc::clone(template));
            }
        }

        if !matching_exact.is_empty() {
            return matching_exact;
        } else if !matching_coerce.is_empty() {
            return matching_coerce;
        } else if allow_coerce && matching_name >= 1 {
            // Return the template with the same name even if it doesn't match the type hint
            return self.templates.iter().filter(|&template| template.borrow().name.source() == name).map(Rc::clone)
                .collect();
        }

        if recursion.should_recurse() {
            let result = self.parent.as_ref().and_then(Weak::upgrade).map(|parent| parent
                .borrow().find_template(name, template_type, allow_coerce, recursion.decrement())).unwrap_or_else(Vec::new);

            if !result.is_empty() {
                return result;
            }
        }

        if recursion.should_recurse() {
            let result = self.imports.as_ref().map(Rc::clone).map(|imports| imports
                .borrow().find_template(name, template_type, allow_coerce, RecursionToParent::Never)).unwrap_or_else(Vec::new);

            if !result.is_empty() {
                return result;
            }
        }

        vec![]
    }

    pub fn find_templates_with_name(&self, name: &str, recursion: RecursionToParent) -> Vec<Rc<RefCell<TemplateDeclaration>>> {
        let matching: Vec<_> = self.templates.iter()
            .filter(|&template| template.borrow().name.source() == name)
            .map(Rc::clone).collect();

        if !matching.is_empty() {
            matching
        } else if recursion.should_recurse() {
            self.parent.as_ref().and_then(|parent| parent.upgrade()).map(|parent| parent
                .borrow().find_templates_with_name(name, recursion.decrement())).unwrap_or_else(Vec::new)
        } else {
            vec![]
        }
    }

    pub fn find_module(&self, name: &str, recursion: RecursionToParent) -> Option<Rc<RefCell<ModuleDeclaration>>> {
        let found = self.modules.get(name).map(Rc::clone);

        if recursion.should_recurse() {
            found.or_else(|| self.parent.as_ref().and_then(|parent| parent.upgrade())
                .and_then(|parent| parent.borrow().find_module(name, recursion.decrement())))
                .or_else(|| self.imports.as_ref().map(Rc::clone)
                    .and_then(|imports| imports.borrow().find_module(name, RecursionToParent::Never)))
        } else {
            found
        }
    }

    pub fn find_module_type_environment(&self, name: &str, recursion: RecursionToParent) -> Option<Rc<RefCell<TypeEnvironment>>> {
        let found = self.children.get(name).map(Rc::clone);

        if recursion.should_recurse() {
            found.or_else(|| self.parent.as_ref().and_then(|parent| parent.upgrade())
                .and_then(|parent| parent.borrow().find_module_type_environment(name, recursion.decrement())))
                .or_else(|| self.imports.as_ref().map(Rc::clone)
                .and_then(|imports| imports.borrow().find_module_type_environment(name, RecursionToParent::Never)))
        } else {
            found
        }
    }

    pub fn find_variable(&self, name: &str, recursion: RecursionToParent) -> Option<Rc<RefCell<VariableDeclaration>>> {
        let found = self.variables.get(name).map(Rc::clone);

        if recursion.should_recurse() {
            found.or_else(|| self.parent.as_ref().and_then(|parent| parent.upgrade())
                .and_then(|parent| parent.borrow().find_variable(name, recursion.decrement())))
                .or_else(|| self.imports.as_ref().map(Rc::clone)
                .and_then(|imports| imports.borrow().find_variable(name, RecursionToParent::Never)))
        } else {
            found
        }
    }

    pub fn put_template(&mut self, name: Token, template: Rc<RefCell<TemplateDeclaration>>) -> bool {
        let template_borrow = template.borrow();
        let template_type = TemplateType {
            this: template_borrow.this.as_ref().map(|token| Box::new(token.expr_type.to_type_hint())),
            args: template_borrow.args.iter().map(|token| token.expr_type.to_type_hint()).collect::<Vec<TypeHint>>(),
            return_type: Box::new(template_borrow.return_type.to_type_hint()),
        };

        let found = self.find_template(name.source(), &template_type, false, RecursionToParent::Never).is_empty()
            || self.imports.as_ref().map(|imports|
            imports.borrow().find_template(name.source(), &template_type, false, RecursionToParent::Never).is_empty()).unwrap_or(false);

        drop(template_borrow);

        if !found {
            false
        } else {
            self.templates.push(template);
            true
        }
    }

    pub fn has_variable(&self, name: &str, recursion: RecursionToParent) -> bool {
        let found = self.variables.contains_key(name);

        if !found && recursion.should_recurse() {
            self.parent.as_ref().and_then(|parent| parent.upgrade())
                .map(|parent| parent.borrow().has_variable(name, recursion.decrement()))
                .unwrap_or(false)
        } else {
            found
        }
    }

    pub fn put_variable(&mut self, name: Token, variable: Rc<RefCell<VariableDeclaration>>) -> bool {
        if self.has_variable(name.source(), RecursionToParent::Never) || self.imports.as_ref().map(|imports|
            imports.borrow().has_variable(name.source(), RecursionToParent::Never)).unwrap_or(false) {
            return false;
        }

        self.variables.insert(name.source().to_owned(), variable).is_none()
    }

    fn has_module(&self, name: &str, recursion: RecursionToParent) -> bool {
        let found = self.modules.contains_key(name);

        if !found && recursion.should_recurse() {
            self.parent.as_ref().and_then(|parent| parent.upgrade())
                .map(|parent| parent.borrow().has_variable(name, recursion.decrement()))
                .unwrap_or(false)
        } else {
            found
        }
    }

    pub fn put_module(&mut self, name: Token, module: Rc<RefCell<ModuleDeclaration>>) -> bool {
        if self.has_module(name.source(), RecursionToParent::Never) || self.imports.as_ref().map(|imports|
            imports.borrow().has_module(name.source(), RecursionToParent::Never)).unwrap_or(false) {
            return false;
        }

        self.modules.insert(name.source().to_owned(), module).is_none()
    }

    pub fn add_child(&mut self, name: String, environment: Rc<RefCell<TypeEnvironment>>) -> bool {
        self.children.insert(name, environment).is_none()
    }

    pub fn get_child(&self, name: &str) -> Option<Rc<RefCell<TypeEnvironment>>> {
        self.children.get(name).map(Rc::clone)
    }
}

#[derive(Clone, PartialEq)]
struct TemplateScope {
    pub this: Option<ExprType>,
    pub args: HashMap<String, ExprType>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum ResolutionError {
    TemplateNotFound(TypeHint),
    TemplateAmbiguous(TypeHint),
    ReferenceAmbiguous(TypeHint),
    ReferenceNotFound(TypeHint),
    ModuleNotFound,
    VariableNotFound,
}

pub struct TypeChecker {
    // global_environment: Rc<RefCell<TypeEnvironment>>,
    environment: Rc<RefCell<TypeEnvironment>>,
    template_scope: Option<TemplateScope>,

    path: PathBuf,

    had_error: bool, panic_mode: bool,
    _config: Rc<Config>,
}

impl TypeChecker {
    pub fn new(path: PathBuf, config: Rc<Config>) -> TypeChecker {
        let environment = TypeEnvironment::new_global();

        TypeChecker {
            /* global_environment: Rc::clone(&environment), */ environment,
            template_scope: None,
            path,
            had_error: false, panic_mode: false,
            _config: config,
        }
    }

    pub fn check_types(&mut self, declarations: Vec<Decl>) -> Vec<TypedDecl> {
        for decl in &declarations {
            self.declare_declaration(decl);
        }

        let mut typed_declarations = Vec::new();

        for decl in declarations {
            typed_declarations.push(self.check_declaration(decl));
        }

        typed_declarations
    }

    fn declare_declaration(&mut self, decl: &Decl) {
        match decl {
            Decl::Template { name, this, args, return_type, expr: _ } => {
                if !self.environment.borrow_mut().put_template(name.clone(),
                    Rc::new(RefCell::new(TemplateDeclaration {
                        name: name.clone(), this: this.clone(), args: args.clone(), return_type: return_type.clone()
                    }))) {
                    self.error_at(*name.start(), &format!("Template '{}' with same parameter list already exists", name.source()), false);
                }
            },
            Decl::Variable { name, expr_type, expr: _, kind } => {
                if !self.environment.borrow_mut().put_variable(name.clone(),
                    Rc::new(RefCell::new(VariableDeclaration {
                        name: name.clone(), expr_type: expr_type.clone(), kind: *kind
                    }))) {
                    self.error_at(*name.start(), &format!("Variable '{}' already exists", name.source()), false);
                }
            },
            Decl::Module { name, declarations } => {
                if self.import_environment().borrow().has_module(name.source(), RecursionToParent::Once) {
                    self.error_at(*name.start(), &format!("Module '{}' already exists", name.source()), false);
                } else {
                    let current_environment = self.environment();
                    let child_environment = TypeEnvironment::new_with_parent(Rc::clone(&current_environment));
                    self.environment = Rc::clone(&child_environment);

                    for decl in declarations {
                        self.declare_declaration(decl);
                    }

                    current_environment.borrow_mut().add_child(name.source().to_owned(), Rc::clone(&child_environment));
                    self.environment = current_environment;

                    self.environment.borrow_mut().put_module(name.clone(), Rc::new(RefCell::new(ModuleDeclaration {
                        name: name.clone(),
                        submodules: child_environment.borrow().modules.clone(),
                        variables: child_environment.borrow().variables.clone(),
                        templates: child_environment.borrow().templates.clone(),
                    })));
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
                self.template_scope = Some(TemplateScope {
                    this: this.as_ref().map(|this| this.expr_type.clone()),
                    args: args.iter().map(|arg| (arg.token.source().to_owned(), arg.expr_type.clone())).collect(),
                });

                let typed_expr = self.check_template_expr(expr, false, return_type.to_type_hint());
                self.template_scope = None;

                let expr_type = typed_expr.get_type();

                if !expr_type.can_coerce_to(&return_type) {
                    self.error_at_with_context(*name.start(), &format!("Mismatched types: expression of template '{}' cannot be converted to the template's return type", name.source()), vec![
                        ("Expected", &return_type.to_type_hint()),
                        ("Found", &expr_type.to_type_hint())
                    ], false);
                }

                let final_return_type = if return_type == ExprType::Any { expr_type } else { return_type.clone() };

                let template_result = self.environment().borrow().find_template(name.source(),
                    &TemplateDeclaration::template_to_template_type(this.as_ref(), &args, &return_type), false, RecursionToParent::Never);

                if template_result.len() != 1 {
                    self.error_at(*name.start(), &format!("Internal compiler error: template '{}' was not declared", name.source()), true);
                } else {
                    template_result[0].borrow_mut().return_type = final_return_type.clone();
                }

                let template_decl = Rc::new(RefCell::new(TypedTemplateDecl {
                    name, this, args, return_type: final_return_type, expr: typed_expr,
                }));

                TypedDecl::Template(template_decl)
            },
            Decl::Variable { name, expr_type, expr, kind } => {
                let typed_expr = self.check_expr(expr, false, expr_type.to_type_hint());
                let actual_expr_type = typed_expr.get_type();

                if !actual_expr_type.can_coerce_to(&expr_type) {
                    self.error_at_with_context(*name.start(), &format!("Mismatched types: expression of variable '{}' cannot be converted to the declared type", name.source()), vec![
                        ("Expected", &expr_type.to_type_hint()),
                        ("Found", &actual_expr_type.to_type_hint())
                    ], false);
                }

                let final_expr_type = if expr_type == ExprType::Any { actual_expr_type } else { expr_type };

                match self.environment().borrow().find_variable(name.source(), RecursionToParent::Never) {
                    Some(variable_decl) => variable_decl.borrow_mut()
                        .expr_type = final_expr_type.clone(),
                    _ => self.error_at(*name.start(), &format!("Internal compiler error: variable '{}' was not declared", name.source()), true),
                };

                let variable = Rc::new(RefCell::new(TypedVariableDecl {
                    name,
                    expr_type: final_expr_type,
                    expr: typed_expr,
                    kind,
                }));

                TypedDecl::Variable(variable)
            },
            Decl::Module { name, declarations } => {
                let current_environment = self.environment();
                let child_environment = current_environment.borrow().get_child(name.source())
                    .expect("Internal compiler error: Missing module in type environment");
                self.environment = Rc::clone(&child_environment);

                let typed_declarations = declarations.into_iter().map(|decl| self.check_declaration(decl)).collect();

                self.environment = current_environment;

                let module = Rc::new(RefCell::new(TypedModuleDecl {
                    name,
                    declarations: typed_declarations,
                }));

                TypedDecl::Module(module)
            },
            Decl::Include { path, declarations } => {
                let mut path_buf = PathBuf::from(path.source());
                std::mem::swap(&mut self.path, &mut path_buf);
                let typed = self.check_types(declarations);
                std::mem::swap(&mut self.path, &mut path_buf);

                TypedDecl::Include { path, declarations: typed }
            },
            Decl::Import { path, selector } => {
                let mut environment = self.environment();

                for name in &path {
                    let result = environment.borrow().find_module_type_environment(name.source(), RecursionToParent::Always);

                    match result {
                        Some(result) => environment = result,
                        None => {
                            self.error_at(*name.start(), &format!("Module '{}' not found", name.source()), false);
                        },
                    }
                }

                let import_environment = self.import_environment();
                let environment_borrow = environment.borrow();
                let mut import_environment_borrow = import_environment.borrow_mut();

                if let Some(selector) = &selector {
                    for name in selector {
                        let templates = environment_borrow.find_templates_with_name(name.source(), RecursionToParent::Never);
                        let variable = environment_borrow.find_variable(name.source(), RecursionToParent::Never);
                        let module = environment_borrow.find_module(name.source(), RecursionToParent::Never);
                        let child = environment_borrow.get_child(name.source());

                        for template in templates {
                            if !import_environment_borrow.put_template(name.clone(), template) {
                                self.error_at(*name.start(), &format!("Template '{}' with same parameter list already exists", name.source()), false);
                            }
                        }

                        if let Some(variable) = variable {
                            if !import_environment_borrow.put_variable(name.clone(), variable) {
                                self.error_at(*name.start(), &format!("Variable '{}' already exists", name.source()), false);
                            }
                        }

                        if let Some(module) = module {
                            if let Some(child) = child {
                                if !import_environment_borrow.put_module(name.clone(), module) {
                                    self.error_at(*name.start(), &format!("Module '{}' already exists", name.source()), false);
                                } else {
                                    import_environment_borrow.add_child(name.source().to_owned(), child);
                                }
                            }
                        }
                    }
                } else {
                    // Add all members of the source environment to self.import_environment()
                    import_environment_borrow.modules.extend(environment_borrow.modules.iter().map(|(k, v)| (k.clone(), Rc::clone(v))));
                    import_environment_borrow.variables.extend(environment_borrow.variables.iter().map(|(k, v)| (k.clone(), Rc::clone(v))));
                    import_environment_borrow.templates.extend(environment_borrow.templates.iter().map(Rc::clone));
                    import_environment_borrow.children.extend(environment_borrow.children.iter().map(|(k, v)| (k.clone(), Rc::clone(v))));
                }

                TypedDecl::Import {
                    path, selector,
                }
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
                let typed_condition = self.check_expr(condition, true, TypeHint::Simple(SimpleType::Boolean));
                let typed_then = self.check_template_expr(*then, static_expr, type_hint.clone());
                let typed_otherwise = self.check_template_expr(*otherwise, static_expr, type_hint);

                if !typed_condition.get_type().can_coerce_to(&ExprType::Boolean) {
                    self.error_at_with_context(*token.start(), "Mismatched types: condition of 'if' expression must be of type 'boolean'", vec![
                        ("Expected", &TypeHint::Simple(SimpleType::Boolean)),
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
            Expr::Identifier(token) => {
                if let Some(template_scope) = &self.template_scope {
                    let arg_type = if token.token_type() == TokenType::This {
                        match &template_scope.this {
                            None => {
                                self.error_at(*token.start(), "Template does not have 'this' parameter", false);
                                Some(ExprType::Error)
                            },
                            Some(this) => Some(this.clone()),
                        }
                    } else {
                        template_scope.args.get(token.source()).cloned()
                    };

                    if let Some(arg_type) = arg_type {
                        return TypedExpr::TemplateArgument {
                            token,
                            expr_type: arg_type,
                        };
                    }
                }

                self.resolve_reference(&token, type_hint)
                    .map(|decl| {
                        let expr_type = match &decl {
                            TypedImportableDecl::Template(template) =>
                                ExprType::Template {
                                    this: template.borrow().this.as_ref().map(|this| Box::new(this.expr_type.clone())),
                                    args: template.borrow().args.iter().map(|arg| arg.expr_type.clone()).collect(),
                                    return_type: Box::new(template.borrow().return_type.clone()),
                                },
                            TypedImportableDecl::Module(_) => ExprType::Module,
                            TypedImportableDecl::Variable(variable) =>
                                variable.borrow().expr_type.clone(),
                        };

                        TypedExpr::Identifier {
                            token,
                            reference: decl,
                            expr_type,
                        }
                    }).unwrap_or(TypedExpr::Error) // Error gets reported in self.resolve_reference
            },
            Expr::UnaryOperator { operator, expr } => {
                self.check_operator_expr(operator, vec![*expr], static_expr, type_hint)
            },
            Expr::BinaryOperator { left, operator, right } => {
                self.check_operator_expr(operator, vec![*left, *right], static_expr, type_hint)
            },
            Expr::FunctionCall { callee, token, args } => {
                /* let has_this = match &callee {
                    Expr::Member { .. } => true,
                    _ => false,
                }; */
                let arg_count = args.len();

                let mut template_type_hint = self.get_template_call_type_hint(&callee, arg_count, type_hint.clone(), static_expr);

                let mut typed_args = Vec::new();

                for arg in args {
                    typed_args.push(self.check_expr(arg, static_expr, template_type_hint.args.swap_remove(0)));
                }

                let searching_template_type = TypeHint::Template(TemplateType {
                    this: template_type_hint.this.clone(),
                    args: typed_args.iter().map(|arg| arg.get_type().to_type_hint()).collect(),
                    return_type: template_type_hint.return_type,
                });

                let typed_callee = self.check_expr(*callee, static_expr, searching_template_type.clone());
                let callee_type = typed_callee.get_type();

                match &callee_type {
                    ExprType::Template { this: _, args, return_type } => {
                        if typed_args.len() != args.len() {
                            self.error_at_with_context(*token.start(), &format!("Wrong number of arguments for template call: expected {}, found {}", args.len(), typed_args.len()), vec![
                                ("Expected", &callee_type.to_type_hint()),
                                ("Found", &searching_template_type),
                            ], false);
                        }

                        for (i, (typed_arg, arg_type)) in typed_args.iter().zip(args.iter()).enumerate() {
                            if !typed_arg.get_type().can_coerce_to(arg_type) {
                                self.error_at_with_context(*token.start(), &format!("Mismatched types: argument {} of template call", i + 1), vec![
                                    ("Expected", &arg_type.to_type_hint()),
                                    ("Found", &typed_arg.get_type().to_type_hint()),
                                ], false);
                            }
                        }

                        TypedExpr::FunctionCall {
                            callee: Box::new(typed_callee),
                            token,
                            args: typed_args,
                            result_type: (**return_type).clone(),
                        }
                    },
                    ExprType::Error => TypedExpr::Error, // To avoid "cannot call non-template expression" error
                    _ => {
                        self.error_at_with_context(*token.start(), "Cannot call non-template expression", vec![
                            ("Expected", &TypeHint::Template(TemplateType::new_any(false, arg_count, type_hint))),
                            ("Found", &callee_type.to_type_hint()),
                        ], true);
                        TypedExpr::Error
                    },
                }
            },
            Expr::Member { receiver, name } => {
                if static_expr {
                    todo!("Static member expressions")
                }

                let typed_receiver = self.check_expr(*receiver, static_expr, TypeHint::Options(vec![TypeHint::Module]));

                match &type_hint {
                    TypeHint::Template(template) => {
                        let templates = match self.resolve_module_typed_expr(&typed_receiver) {
                            Ok(module) => module.borrow().find_template(name.source(), template, true, RecursionToParent::Always),
                            Err(_) => self.environment().borrow().find_template(name.source(), template, true, RecursionToParent::Always),
                        };

                        if templates.is_empty() {
                            self.error_at_with_context(*name.start(), &format!("Template '{}' not found", name.source()), vec![
                                ("Expected", &type_hint),
                            ], false);
                            TypedExpr::Error
                        } else if templates.len() > 1 {
                            self.error_at_with_context(*name.start(), &format!("Template '{}' is ambiguous", name.source()), vec![
                                ("Expected", &type_hint),
                            ], false);
                            TypedExpr::Error
                        } else {
                            let template_expr_type = TemplateDeclaration::decl_to_type(Rc::clone(&templates[0]));

                            // Template can't have this parameter when using member expression syntax
                            if let ExprType::Template { this, .. } = &template_expr_type {
                                if let Some(hint_this) = template.this.as_ref() {
                                    if let Some(this) = this.as_ref() {
                                        if !hint_this.can_coerce_from(this, true) {
                                            self.error_at_with_context(*name.start(), &format!("Mismatched types: 'this' argument of template '{}'", name.source()), vec![
                                                ("Expected", &**hint_this),
                                                ("Found", &this.to_type_hint()),
                                            ], false);
                                        }
                                    }
                                }
                            };

                            TypedExpr::Member {
                                receiver: Box::new(typed_receiver),
                                name,
                                result_type: template_expr_type,
                            }
                        }
                    },
                    _ => {
                        self.error_at_with_context(*name.start(), &format!("Reference '{}' not found", name.source()), vec![
                            ("Expected", &type_hint),
                        ], false);
                        TypedExpr::Error
                    },
                }
            },
            Expr::Receiver { receiver, name } => {
                let typed_receiver = self.check_expr(*receiver, static_expr, TypeHint::Any);
                let receiver_type = typed_receiver.get_type();

                match &type_hint {
                    TypeHint::Template(template) => {
                        let templates = self.environment().borrow().find_template(name.source(), &TemplateType { this: Some(Box::new(receiver_type.to_type_hint())), .. template.clone() }, true, RecursionToParent::Always);

                        if templates.is_empty() {
                            self.error_at_with_context(*name.start(), &format!("Template '{}' not found", name.source()), vec![
                            ("Expected", &type_hint),
                            ], false);
                            TypedExpr::Error
                        } else if templates.len() > 1 {
                            self.error_at_with_context(*name.start(), &format!("Template '{}' is ambiguous", name.source()), vec![
                            ("Expected", &type_hint),
                            ], false);
                            TypedExpr::Error
                        } else {
                            let template_expr_type = TemplateDeclaration::decl_to_type(Rc::clone(&templates[0]));

                            // Template will always have 'this' parameter when using receiver syntax
                            if let ExprType::Template { this, .. } = &template_expr_type {
                                if let Some(hint_this) = template.this.as_ref() {
                                    if let Some(this) = this.as_ref() {
                                        if !hint_this.can_coerce_from(this, true) {
                                            self.error_at_with_context(*name.start(), &format!("Mismatched types: 'this' argument of template '{}'", name.source()), vec![
                                            ("Expected", &**hint_this),
                                            ("Found", &this.to_type_hint()),
                                            ], false);
                                        }
                                    }
                                }
                            };

                            TypedExpr::Receiver {
                                receiver: Box::new(typed_receiver),
                                name,
                                result_type: template_expr_type,
                            }
                        }
                    },
                    _ => {
                        if name.source() == "type" {
                            return TypedExpr::BuiltinType(receiver_type);
                        }

                        self.error_at_with_context(*name.start(), &format!("Reference '{}' not found", name.source()), vec![
                        ("Expected", &type_hint),
                        ], false);
                        TypedExpr::Error
                    },
                }
            },
            Expr::Index { receiver, operator, index } => {
                self.check_operator_expr(operator, vec![*receiver, *index], static_expr, type_hint)
            },
            Expr::BuiltinFunctionCall { name, mut args } => {
                if name.source() == "static" {
                    if args.len() != 1 {
                        self.error_at(*name.start(), &format!("Wrong number of arguments for built-in function call: expected 1, found {}", args.len()), false);
                        TypedExpr::Error
                    } else {
                        self.check_expr(args.swap_remove(0), true, type_hint)
                    }
                } else if name.source() == "nonstatic" {
                    if args.len() != 1 {
                        self.error_at(*name.start(), &format!("Wrong number of arguments for built-in function call: expected 1, found {}", args.len()), false);
                        TypedExpr::Error
                    } else {
                        self.check_expr(args.swap_remove(0), false, type_hint)
                    }
                } else if name.source() == "error" {
                    if args.len() < 1 {
                        self.error_at(*name.start(), &format!("Wrong number of arguments for built-in function call: expected at least 1, found {}", args.len()), false);
                        TypedExpr::Error
                    } else {
                        let typed_args = args.into_iter().enumerate()
                            .map(|(i, arg)| self.check_expr(arg, static_expr, if i == 0 { TypeHint::Simple(SimpleType::String) } else { TypeHint::Any }))
                            .collect::<Vec<_>>();

                        if typed_args[0].get_type() != ExprType::String {
                            self.error_at_with_context(*name.start(), "Mismatched types: argument 1 of built-in function call", vec![
                                ("Expected", &TypeHint::Simple(SimpleType::String)),
                                ("Found", &typed_args[0].get_type().to_type_hint()),
                            ], false);
                        }

                        TypedExpr::BuiltinFunctionCall { name, args: typed_args, result_type: ExprType::Any }
                    }
                } else {
                    self.error_at(*name.start(), &format!("Built-in function '{}' not found", name.source()), true);
                    TypedExpr::Error
                }
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

    fn check_operator_expr(&mut self, operator: Token, args: Vec<Expr>, static_expr: bool, return_type_hint: TypeHint) -> TypedExpr {
        if static_expr {
            if let Some(result) = self.check_static_operator_expr(&operator, &args, &return_type_hint) {
                return result;
            }
        }

        let mut template_type_hint = TemplateType::merge(
            &TemplateDeclaration::templates_to_template_types(
                &self.environment.borrow().find_template_options(operator.source(), false, args.len(), RecursionToParent::Always)),
            false, args.len(), return_type_hint);

        let mut typed_args = Vec::new();

        for arg in args {
            typed_args.push(self.check_expr(arg, static_expr, template_type_hint.args.swap_remove(0)));
        }

        let args_types: Vec<TypeHint> = typed_args.iter().map(|arg| arg.get_type().to_type_hint()).collect();
        let searching_template_type = TypeHint::Template(TemplateType {
            this: None,
            args: args_types.clone(),
            return_type: template_type_hint.return_type,
        });

        match self.resolve_reference(&operator, searching_template_type.clone()) {
            Some(TypedImportableDecl::Template(template)) => {
                let template_expr_type = TemplateDeclaration::decl_to_type(Rc::clone(&template));
                let result_type = template.borrow().return_type.clone();

                match &template_expr_type {
                    ExprType::Template { this, args: found_args, return_type: _ } => {
                        if this.is_some() {
                            self.error_at_with_context(*operator.start(), &format!("Resolved operator template '{}' has a 'this' parameter", operator.source()), vec![
                                ("Expected", &searching_template_type),
                                ("Found", &template_expr_type.to_type_hint()),
                            ], false);
                        } else if typed_args.len() != found_args.len() {
                            self.error_at_with_context(*operator.start(), &format!("Wrong number of parameters to call operator template '{}': expected {}, found {}", operator.source(), found_args.len(), typed_args.len()), vec![
                                ("Expected", &template_expr_type.to_type_hint()),
                                ("Found", &searching_template_type),
                            ], false);
                        } else {
                            for (i, (searching_arg, found_arg)) in args_types.iter().zip(found_args.iter()).enumerate() {
                                if !searching_arg.can_coerce_to(found_arg, true) {
                                    self.error_at_with_context(*operator.start(), &format!("Mismatched types: argument {} of operator template '{}'", i + 1, operator.source()), vec![
                                        ("Expected", &found_arg.to_type_hint()),
                                        ("Found", searching_arg),
                                    ], false);
                                }
                            }
                        }
                    },
                    _ => self.error_at(*operator.start(), "Internal compiler error: Resolved template is not a template", true),
                };

                TypedExpr::FunctionCall {
                    callee: Box::new(TypedExpr::Identifier {
                        token: operator.clone(),
                        reference: TypedImportableDecl::Template(Rc::clone(&template)),
                        expr_type: template_expr_type,
                    }),
                    token: operator,
                    args: typed_args,
                    result_type,
                }
            },
            None => TypedExpr::Error, // Error was already reported in self.resolve_reference
            _ => {
                self.error_at(*operator.start(), "Internal compiler error: Resolved reference is not a template", true);
                TypedExpr::Error
            },
        }
    }

    fn check_static_operator_expr(&mut self, operator: &Token, args: &[Expr], _return_type_hint: &TypeHint) -> Option<TypedExpr> {
        let arg_hint = match args.len() {
            1 => match operator.source() {
                "+" | "-" => TypeHint::Options(vec![TypeHint::Simple(SimpleType::Int), TypeHint::Simple(SimpleType::Float)]),
                "!" => TypeHint::Simple(SimpleType::Boolean),
                _ => return None,
            },
            2 => match operator.source() {
                "+" | "-" | "*" | "/" | "<" | "<=" | ">" | ">=" =>
                    TypeHint::Options(vec![TypeHint::Simple(SimpleType::Int), TypeHint::Simple(SimpleType::Float)]),
                "==" | "!=" => TypeHint::Any,
                "&&" | "||" => TypeHint::Simple(SimpleType::Boolean),
                "[" => TypeHint::Any, // Array and int
                _ => return None,
            },
            _ => return None,
        };

        let arg_hints = if args.len() == 2 && operator.source() == "[" {
            vec![TypeHint::Simple(SimpleType::Array), TypeHint::Simple(SimpleType::Int)]
        } else {
            vec![arg_hint; args.len()]
        };

        let mut typed_args = Vec::new();

        for (i, arg) in args.iter().enumerate() {
            typed_args.push(self.check_expr(arg.clone(), true, arg_hints[i].clone()));
        }

        let arg_types: Vec<ExprType> = typed_args.iter().map(|arg| arg.get_type()).collect();

        for (i, (arg_hint, arg_type)) in arg_hints.iter().zip(arg_types.iter()).enumerate() {
            if !arg_hint.can_coerce_from(arg_type, true) {
                self.error_at_with_context(*operator.start(), &format!("Mismatched types: argument {} for static operator '{}'", i + 1, operator.source()), vec![
                    ("Expected", arg_hint),
                    ("Found", &arg_type.to_type_hint()),
                ], false);
            }
        }

        let result_type = match args.len() {
            1 => match operator.source() {
                "+" | "-" => if arg_types.iter().any(|arg_type| *arg_type == ExprType::Float) {
                    ExprType::Float
                } else {
                    ExprType::Int
                },
                "!" => ExprType::Boolean,
                _ => ExprType::Any,
            },
            2 => match operator.source() {
                "+" | "-" | "*" | "/" => if arg_types.iter().any(|arg_type| *arg_type == ExprType::Float) {
                    ExprType::Float
                } else {
                    ExprType::Int
                },
                "<" | "<=" | ">" | ">=" | "==" | "!=" => ExprType::Boolean,
                "&&" | "||" => ExprType::Boolean,
                "[" => ExprType::Any,
                _ => ExprType::Any,
            },
            _ => ExprType::Any,
        };

        Some(TypedExpr::BuiltinFunctionCall {
            name: operator.clone(),
            args: typed_args,
            result_type,
        })
    }

    fn get_template_call_type_hint(&mut self, callee: &Expr, arg_count: usize, return_type_hint: TypeHint, _static_expr: bool) -> TemplateType {
        match callee {
            Expr::Identifier(token) =>
                TemplateType::merge(&TemplateDeclaration::templates_to_template_types(
                    &self.environment.borrow().find_template_options(token.source(), false, arg_count, RecursionToParent::Always)),
                    false, arg_count, return_type_hint),
            Expr::FunctionCall { callee: callee2, token: _, args } => {
                let arg_count2 = args.len();

                let template_type_hint = self.get_template_call_type_hint(callee2, arg_count2,
                    TypeHint::Template(TemplateType::new_any(false, arg_count, return_type_hint.clone())), _static_expr);

                match *template_type_hint.return_type {
                    TypeHint::Template(template) => template,
                    TypeHint::Options(hints) => {
                        if hints.is_empty() {
                            TemplateType::new_any(false, arg_count, return_type_hint)
                        } else {
                            TemplateType::merge(&hints.into_iter()
                                .filter_map(|hint| if let TypeHint::Template(template) = hint { Some(template) } else { None })
                                .collect::<Vec<_>>(), false, arg_count, return_type_hint)
                        }
                    },
                    _ => TemplateType::new_any(false, arg_count, return_type_hint),
                }
            },
            Expr::Member { receiver, name } => {
                let environment = match self.resolve_module_expr(&**receiver) {
                    Ok(environment) => environment,
                    Err(_) => return TemplateType::new_any(false, arg_count, return_type_hint),
                };

                let x = TemplateType::merge(&TemplateDeclaration::templates_to_template_types(
                    &environment.borrow().find_template_options(name.source(), false, arg_count, RecursionToParent::Always)),
                    false, arg_count, return_type_hint);
                x
            },
            Expr::Receiver { receiver, name } => {
                let x = TemplateType::merge(&TemplateDeclaration::templates_to_template_types(
                    &self.environment().borrow().find_template_options(name.source(), true, arg_count, RecursionToParent::Always)),
                    true, arg_count, return_type_hint);
                x
            },
            Expr::UnaryOperator { operator, .. } | Expr::BinaryOperator { operator, .. } | Expr::Index { operator, .. } => {
                self.error_at(*operator.start(), "Calling templates returned by operators is not supported yet", true);
                TemplateType::new_any(false, arg_count, return_type_hint)
            },
            _ => TemplateType::new_any(false, arg_count, return_type_hint),
        }
    }

    fn resolve_reference(&mut self, name: &Token, type_hint: TypeHint) -> Option<TypedImportableDecl> {
        match self.resolve_reference_silent(name, type_hint) {
            Ok(result) => Some(result),
            Err(reason) => match reason {
                ResolutionError::TemplateNotFound(type_hint) => {
                    self.error_at_with_context(*name.start(), &format!("Template '{}' not found", name.source()), vec![
                        ("Expected", &type_hint),
                    ], false);
                    None
                },
                ResolutionError::TemplateAmbiguous(type_hint) => {
                    self.error_at_with_context(*name.start(), &format!("Reference to template '{}' is ambiguous", name.source()), vec![
                        ("Expected", &type_hint),
                    ], false);
                    None
                },
                ResolutionError::ReferenceNotFound(type_hint) => {
                    self.error_at_with_context(*name.start(), &format!("Reference '{}' not found", name.source()), vec![
                        ("Expected", &type_hint),
                    ], false);
                    None
                },
                ResolutionError::ReferenceAmbiguous(type_hint) => {
                    self.error_at_with_context(*name.start(), &format!("Reference '{}' is ambiguous", name.source()), vec![
                        ("Expected", &type_hint),
                    ], false);
                    None
                },
                ResolutionError::ModuleNotFound => {
                    self.error_at(*name.start(), &format!("Module '{}' not found", name.source()), false);
                    None
                },
                ResolutionError::VariableNotFound => {
                    self.error_at(*name.start(), &format!("Variable '{}' not found", name.source()), false);
                    None
                },
            },
        }
    }

    fn resolve_reference_silent(&mut self, name: &Token, type_hint: TypeHint) -> Result<TypedImportableDecl, ResolutionError> {
        match type_hint {
            TypeHint::Template(template_type) => {
                let mut templates = self.resolve_template(name, &template_type);

                if templates.is_empty() {
                    Err(ResolutionError::TemplateNotFound(TypeHint::Template(template_type)))
                } else if templates.len() > 1 {
                    Err(ResolutionError::TemplateAmbiguous(TypeHint::Template(template_type)))
                } else {
                    Ok(TypedImportableDecl::Template(templates.swap_remove(0)))
                }
            },
            TypeHint::Module => {
                match self.resolve_module(name) {
                    Some(module) => Ok(TypedImportableDecl::Module(module)),
                    None => Err(ResolutionError::ModuleNotFound),
                }
            },
            TypeHint::Options(options) => {
                let mut results: Vec<TypedImportableDecl> = options.clone().into_iter()
                    .map(|option| self.resolve_reference_silent(name, option).ok())
                    .flat_map(|result| result.into_iter())
                    .collect();

                if results.is_empty() {
                    Err(ResolutionError::ReferenceNotFound(TypeHint::Options(options)))
                } else if results.len() > 1 {
                    Err(ResolutionError::ReferenceAmbiguous(TypeHint::Options(options)))
                } else {
                    Ok(results.swap_remove(0))
                }
            },
            _ => {
                match self.resolve_variable(name) {
                    Some(variable) => {
                        Ok(TypedImportableDecl::Variable(variable))
                        // Even if type hint doesn't match
                    }
                    None => Err(ResolutionError::VariableNotFound),
                }
            },
        }
    }

    fn resolve_template(&mut self, name: &Token, template_type: &TemplateType) -> Vec<Rc<RefCell<TemplateDeclaration>>> {
        self.environment().borrow()
            .find_template(name.source(), template_type, true, RecursionToParent::Always).into_iter()
            .filter(|template|
                template_type.return_type.can_coerce_from(&template.borrow().return_type, true))
            .collect()
    }

    fn resolve_module(&self, name: &Token) -> Option<Rc<RefCell<ModuleDeclaration>>> {
        self.environment().borrow()
            .find_module(name.source(), RecursionToParent::Always)
    }

    fn resolve_module_typed_expr<'a>(&self, expr: &'a TypedExpr) -> Result<Rc<RefCell<TypeEnvironment>>, Option<&'a Token>> {
        match expr {
            TypedExpr::Identifier { token, expr_type, .. } => {
                if *expr_type != ExprType::Module {
                    return Err(Some(token));
                }

                self.environment().borrow().get_child(token.source())
                    .or_else(|| self.environment.borrow().get_child(token.source())).ok_or(Some(token))
            },
            TypedExpr::Member { receiver, name, result_type } => {
                if *result_type != ExprType::Module {
                    return Err(Some(name));
                }

                let receiver_module = self.resolve_module_typed_expr(receiver)?;
                let x = receiver_module.borrow().get_child(name.source()).ok_or(Some(name));
                x
            },
            _ => Err(None),
        }
    }

    fn resolve_module_expr<'a>(&self, expr: &'a Expr) -> Result<Rc<RefCell<TypeEnvironment>>, Option<&'a Token>> {
        match expr {
            Expr::Identifier(token) => {
                self.environment().borrow().get_child(token.source())
                    .or_else(|| self.environment.borrow().get_child(token.source())).ok_or(Some(token))
            },
            Expr::Member { receiver, name } => {
                let receiver_module = self.resolve_module_expr(receiver)?;
                let x = receiver_module.borrow().get_child(name.source()).ok_or(Some(name));
                x
            },
            _ => Err(None),
        }
    }

    fn resolve_variable(&self, name: &Token) -> Option<Rc<RefCell<VariableDeclaration>>> {
        self.environment().borrow()
            .find_variable(name.source(), RecursionToParent::Always)
    }

    fn environment(&self) -> Rc<RefCell<TypeEnvironment>> {
        Rc::clone(&self.environment)
    }

    fn import_environment(&self) -> Rc<RefCell<TypeEnvironment>> {
        TypeEnvironment::get_or_create_import_environment(self.environment())
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

#[cfg(test)]
mod tests;
