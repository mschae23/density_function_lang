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
    children: HashMap<String, Rc<RefCell<TypeEnvironment>>>,
}

impl TypeEnvironment {
    pub fn new_global() -> Rc<RefCell<TypeEnvironment>> {
        Rc::new(RefCell::new(TypeEnvironment {
            templates: Vec::new(),
            modules: HashMap::new(),
            variables: HashMap::new(),

            parent: None, children: HashMap::new(),
        }))
    }

    pub fn new_with_parent(parent: Rc<RefCell<TypeEnvironment>>) -> Rc<RefCell<TypeEnvironment>> {
        Rc::new(RefCell::new(TypeEnvironment {
            templates: Vec::new(),
            modules: HashMap::new(),
            variables: HashMap::new(),

            parent: Some(Rc::downgrade(&parent)), children: HashMap::new(),
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

    pub fn find_template_options(&self, name: &str, this: bool, arg_count: usize, recurse_to_parent: bool) -> Vec<Rc<RefCell<TemplateDeclaration>>> {
        let found: Vec<Rc<RefCell<TemplateDeclaration>>> = self.templates.iter().filter(|&template| {
            let template_borrow = template.borrow();

            template_borrow.name.source() == name
            && template_borrow.this.is_some() == this
            && template_borrow.args.len() == arg_count
        }).map(Rc::clone).collect();

        if recurse_to_parent && found.is_empty() {
            self.parent.as_ref().and_then(|parent| parent.upgrade()).into_iter().flat_map(|parent| parent
                .borrow().find_template_options(name, this, arg_count, recurse_to_parent).into_iter()).collect()
        } else {
            found
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

            if template_type.can_coerce_from(this.as_ref(), &args, &return_type, false) {
                matching_exact.push(Rc::clone(template));
            } else if allow_coerce && template_type.can_coerce_from(this.as_ref(), &args, &return_type, allow_coerce) {
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

    pub fn has_module(&self, name: &str, recurse_to_parent: bool) -> bool {
        let found = self.modules.contains_key(name);

        if recurse_to_parent && !found {
            self.parent.as_ref().and_then(|parent| parent.upgrade())
                .map(|parent| parent.borrow().has_module(name, recurse_to_parent))
                .unwrap_or(false)
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

    pub fn put_module(&mut self, name: Token, submodules: HashMap<String, Rc<RefCell<ModuleDeclaration>>>,
                      variables: HashMap<String, Rc<RefCell<VariableDeclaration>>>,
                      templates: Vec<Rc<RefCell<TemplateDeclaration>>>) -> bool {
        match self.modules.entry(name.source().to_owned()) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(Rc::new(RefCell::new(ModuleDeclaration {
                    name, submodules, variables, templates })));
                true
            },
        }
    }

    pub fn add_child(&mut self, name: String, environment: Rc<RefCell<TypeEnvironment>>) {
        self.children.insert(name, environment);
    }

    pub fn get_child(&self, name: &str) -> Option<Rc<RefCell<TypeEnvironment>>> {
        self.children.get(name).map(Rc::clone)
    }
}

pub struct TypeChecker {
    global_environment: Rc<RefCell<TypeEnvironment>>,
    environment: Rc<RefCell<TypeEnvironment>>,

    path: PathBuf,

    had_error: bool, panic_mode: bool,
    config: Rc<Config>,
}

impl TypeChecker {
    pub fn new(path: PathBuf, config: Rc<Config>) -> TypeChecker {
        let environment = TypeEnvironment::new_global();

        TypeChecker {
            global_environment: Rc::clone(&environment), environment,
            path,
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
            Decl::Module { name, declarations } => {
                if self.environment.borrow().has_module(name.source(), false) {
                    self.error_at(*name.start(), "Module with duplicate name", false);
                } else {
                    let current_environment = self.environment();
                    let child_environment = TypeEnvironment::new_with_parent(Rc::clone(&current_environment));
                    self.environment = Rc::clone(&child_environment);

                    for decl in declarations {
                        self.declare_declaration(decl);
                    }

                    current_environment.borrow_mut().add_child(name.source().to_owned(), Rc::clone(&child_environment));
                    self.environment = current_environment;

                    self.environment.borrow_mut().put_module(name.clone(),
                        child_environment.borrow().modules.clone(),
                        child_environment.borrow().variables.clone(),
                        child_environment.borrow().templates.clone());
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
                    self.error_at_with_context(*name.start(), &format!("Mismatched types: expression of template '{}' cannot be converted to the template's return type", name.source()), vec![
                        ("Expected", &return_type.to_type_hint()),
                        ("Found", &expr_type.to_type_hint())
                    ], false);
                }

                let final_return_type = if return_type == ExprType::Any { expr_type } else { return_type.clone() };

                match self.environment.borrow().find_template(name.source(),
                    &Self::template_to_template_type(this.as_ref(), &args, &return_type), false, false) {
                    Some(Some(template_decl)) => template_decl.borrow_mut()
                        .return_type = final_return_type.clone(),
                    _ => {}, // Weird, it should be there
                };

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

                match self.environment.borrow().find_variable(name.source(), false) {
                    Some(variable_decl) => variable_decl.borrow_mut()
                        .expr_type = final_expr_type.clone(),
                    _ => {}, // Weird, it should be there
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

    fn template_to_template_type(this: Option<&TypedToken>, args: &[TypedToken], return_type: &ExprType) -> TemplateType {
        TemplateType {
            this: this.map(|this| Box::new(this.expr_type.to_type_hint())),
            args: args.iter().map(|arg| arg.expr_type.to_type_hint()).collect(),
            return_type: Box::new(return_type.to_type_hint()),
        }
    }

    fn templates_to_template_types(templates: &[Rc<RefCell<TemplateDeclaration>>]) -> Vec<TemplateType> {
        templates.iter().map(|template| {
            let template_borrow = template.borrow();

            Self::template_to_template_type(template_borrow.this.as_ref(), &template_borrow.args, &template_borrow.return_type)
        }).collect()
    }

    fn merge_template_types(templates: &[TemplateType], this: bool, arg_count: usize, return_type_hint: TypeHint) -> TemplateType {
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

            let this = if this.len() > 1 {
                TypeHint::Options(this)
            } else if this.len() == 1 {
                this.swap_remove(0)
            } else {
                TypeHint::Any
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

                args.push(if types.len() > 1 {
                    TypeHint::Options(types)
                } else if types.len() == 1 {
                    types.swap_remove(0)
                } else {
                    TypeHint::Any
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

                Box::new(if types.len() > 1 {
                    TypeHint::Options(types)
                } else if types.len() == 1 {
                    types.swap_remove(0)
                } else {
                    TypeHint::Any
                })
            };

            TemplateType {
                this: if has_this { Some(Box::new(this)) } else { None },
                args,
                return_type,
            }
        }
    }

    fn template_decl_to_type(template_decl: Rc<RefCell<TemplateDeclaration>>) -> ExprType {
        let template_borrow = template_decl.borrow();

        ExprType::Template {
            this: template_borrow.this.as_ref().map(|this| Box::new(this.expr_type.clone())),
            args: template_borrow.args.iter().map(|arg| arg.expr_type.clone()).collect(),
            return_type: Box::new(template_borrow.return_type.clone()),
        }
    }

    fn check_expr(&mut self, expr: Expr, static_expr: bool, type_hint: TypeHint) -> TypedExpr {
        match expr {
            Expr::ConstantFloat(value) => TypedExpr::ConstantFloat(value),
            Expr::ConstantInt(value) => TypedExpr::ConstantInt(value),
            Expr::ConstantBoolean(value) => TypedExpr::ConstantBoolean(value),
            Expr::ConstantString(value) => TypedExpr::ConstantString(value),
            Expr::Identifier(token) => {
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
                let mut template_type_hint = Self::merge_template_types(
                    &Self::templates_to_template_types(
                        &self.environment.borrow().find_template_options(operator.source(), false, 1, true)),
                      false, 1, type_hint.clone());

                let typed_expr = self.check_expr(*expr, static_expr, template_type_hint.args.swap_remove(0));

                match self.resolve_reference(&operator, TypeHint::Template(TemplateType {
                    this: None,
                    args: vec![typed_expr.get_type().to_type_hint()],
                    return_type: template_type_hint.return_type,
                })) {
                    Some(TypedImportableDecl::Template(template)) => {
                        let template_expr_type = Self::template_decl_to_type(Rc::clone(&template));
                        let result_type = template.borrow().return_type.clone();

                        TypedExpr::FunctionCall {
                            callee: Box::new(TypedExpr::Identifier {
                                token: operator.clone(),
                                reference: TypedImportableDecl::Template(Rc::clone(&template)),
                                expr_type: template_expr_type,
                            }),
                            token: operator,
                            args: vec![typed_expr],
                            result_type,
                        }
                    },
                    None => TypedExpr::Error, // Error was already reported in self.resolve_reference
                    _ => {
                        self.error_at(*operator.start(), "Internal compiler error: Resolved reference is not a template", true);
                        TypedExpr::Error
                    },
                }
            },
            Expr::BinaryOperator { left, operator, right } => {
                let mut template_type_hint = Self::merge_template_types(
                    &Self::templates_to_template_types(
                        &self.environment.borrow().find_template_options(operator.source(), false, 2, true)),
                    false, 2, type_hint.clone());

                let typed_left = self.check_expr(*left, static_expr, template_type_hint.args.swap_remove(0));
                let typed_right = self.check_expr(*right, static_expr, template_type_hint.args.swap_remove(0));

                match self.resolve_reference(&operator, TypeHint::Template(TemplateType {
                    this: None,
                    args: vec![typed_left.get_type().to_type_hint(), typed_right.get_type().to_type_hint()],
                    return_type: template_type_hint.return_type,
                })) {
                    Some(TypedImportableDecl::Template(template)) => {
                        let template_expr_type = Self::template_decl_to_type(Rc::clone(&template));
                        let result_type = template.borrow().return_type.clone();

                        TypedExpr::FunctionCall {
                            callee: Box::new(TypedExpr::Identifier {
                                token: operator.clone(),
                                reference: TypedImportableDecl::Template(Rc::clone(&template)),
                                expr_type: template_expr_type,
                            }),
                            token: operator,
                            args: vec![typed_left, typed_right],
                            result_type,
                        }
                    },
                    None => TypedExpr::Error, // Error was already reported in self.resolve_reference
                    _ => {
                        self.error_at(*operator.start(), "Internal compiler error: Resolved reference is not a template", true);
                        TypedExpr::Error
                    },
                }
            },
            Expr::FunctionCall { callee, token, args } => {
                /* let has_this = match &callee {
                    Expr::Member { .. } => true,
                    _ => false,
                }; */
                let arg_count = args.len();

                let mut template_type_hint = self.get_template_call_type_hint(&*callee, arg_count, type_hint.clone(), static_expr);

                let mut typed_args = Vec::new();

                for arg in args {
                    typed_args.push(self.check_expr(arg, static_expr, template_type_hint.args.swap_remove(0)));
                }

                let typed_callee = self.check_expr(*callee, static_expr, TypeHint::Template(TemplateType {
                    this: template_type_hint.this.clone(),
                    args: typed_args.iter().map(|arg| arg.get_type().to_type_hint()).collect(),
                    return_type: template_type_hint.return_type,
                }));
                let callee_type = typed_callee.get_type();

                match &callee_type {
                    ExprType::Template { this: _, args, return_type } => {
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
                            result_type: (&**return_type).clone(),
                        }
                    },
                    _ => {
                        self.error_at_with_context(*token.start(), "Cannot call non-template expression", vec![
                            ("Expected", &TypeHint::Template(TemplateType::new_any(false, arg_count, type_hint))),
                            ("Found", &callee_type.to_type_hint()),
                        ], true);
                        TypedExpr::Error
                    },
                }
            },
            Expr::Member { receiver: _, name: _ } => {
                TypedExpr::Error
            },
            Expr::Index { receiver: _, operator: _, index: _ } => {
                TypedExpr::Error
            },
            Expr::BuiltinFunctionCall { name: _, args: _ } => {
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

    fn get_template_call_type_hint(&mut self, callee: &Expr, arg_count: usize, return_type_hint: TypeHint, static_expr: bool) -> TemplateType {
        match callee {
            Expr::Identifier(token) =>
                Self::merge_template_types(&Self::templates_to_template_types(
                    &self.environment.borrow().find_template_options(token.source(), false, arg_count, true)),
                    false, arg_count, return_type_hint),
            Expr::FunctionCall { callee: callee2, token: _, args } => {
                /* let has_this2 = match &callee2 {
                    Expr::Member { .. } => true,
                    _ => false,
                }; */
                let arg_count2 = args.len();

                let template_type_hint = self.get_template_call_type_hint(&*callee2, arg_count2,
                    TypeHint::Template(TemplateType::new_any(false, arg_count, return_type_hint.clone())), static_expr);

                match *template_type_hint.return_type {
                    TypeHint::Template(template) => template,
                    TypeHint::Options(hints) => {
                        if hints.is_empty() {
                            TemplateType::new_any(false, arg_count, return_type_hint)
                        } else {
                            let mut types = Vec::new();

                            for return_type in hints {
                                match return_type {
                                    TypeHint::Template(template) => types.push(template),
                                    _ => {},
                                }
                            }

                            Self::merge_template_types(&types, false, arg_count, return_type_hint)
                        }
                    },
                    _ => TemplateType::new_any(false, arg_count, return_type_hint),
                }
            },
            Expr::Member { receiver: _, name } => {
                self.error_at(*name.start(), "Calling templates on other elements is not supported yet", true);
                TemplateType::new_any(true, arg_count, return_type_hint)
            },
            Expr::UnaryOperator { operator, .. } | Expr::BinaryOperator { operator, .. } | Expr::Index { operator, .. } => {
                self.error_at(*operator.start(), "Calling templates returned by operators is not implemented yet", true);
                TemplateType::new_any(false, arg_count, return_type_hint)
            },
            _ => TemplateType::new_any(false, arg_count, return_type_hint),
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
                        if !type_hint.can_coerce_from(&variable.borrow().expr_type, true) {
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
                        self.error_at(*name.start(), &format!("Variable '{}' cannot be found", name.source()), false);
                        None
                    }
                }
            },
        }
    }

    fn resolve_template(&mut self, name: &Token, template_type: &TemplateType) -> Option<Option<Rc<RefCell<TemplateDeclaration>>>> {
        self.environment.borrow().find_template(name.source(), template_type, true, true)
            .filter(|template| template.as_ref()
                .map(|template| template_type.return_type.can_coerce_to(&template.borrow().return_type, true))
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

#[cfg(test)]
mod tests;
