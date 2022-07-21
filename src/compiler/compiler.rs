use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::{Expr, JsonElement, Module, OutputFunction, Stmt, Template};
use crate::compiler::lexer::{Token, TokenPos};

pub struct Compiler {
    current_module: Vec<Rc<RefCell<Module>>>,
    top_level_module: Rc<RefCell<Module>>,
    template_args: Vec<(String, JsonElement)>,

    path: PathBuf, target_dir: PathBuf,

    had_error: bool, panic_mode: bool,
}

impl Compiler {
    pub fn new(path: PathBuf, target_dir: PathBuf) -> Compiler {
        Compiler {
            current_module: vec![],
            top_level_module: Rc::new(RefCell::new(Module { name: String::from("<top-level>"),
                sub_modules: vec![], templates: vec![], output_functions: vec![] })),
            template_args: vec![],

            path, target_dir,

            had_error: false, panic_mode: false,
        }
    }

    pub fn compile(&mut self, statements: Vec<Stmt>) -> Vec<Rc<RefCell<OutputFunction>>> {
        for stmt in statements {
            self.compile_statement(stmt);
        }

        let mut outputs = vec![];
        Self::collect_output_functions(&mut outputs, Rc::clone(&self.top_level_module));
        outputs
    }

    fn collect_output_functions(outputs: &mut Vec<Rc<RefCell<OutputFunction>>>, module: Rc<RefCell<Module>>) {
        outputs.append(&mut module.borrow_mut().output_functions);

        let module_borrow = module.borrow_mut();

        for sub_module in &module_borrow.sub_modules {
            Self::collect_output_functions(outputs, Rc::clone(sub_module));
        }
    }

    fn compile_statement(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Template { name, args, expr } => self.compile_template(name, args, expr),
            Stmt::Function { name, expr } => self.compile_function(name, expr),
            Stmt::Module { name, statements } => self.compile_module(name, statements),
            Stmt::Include { path } => self.compile_include(path),
            Stmt::Import { path, selector } => self.compile_import(path, selector),
            Stmt::Error => {},
        }
    }

    fn compile_template(&mut self, name: Token, args: Vec<Token>, expr: Expr) {
        let current_module = self.current_module();
        let mut current_module = current_module.borrow_mut();

        if current_module.templates.iter().any(|template| *template.borrow().name == *name.source() && template.borrow().args.len() == args.len()) {
            self.error_at(*name.start(), "Tried to define multiple templates with the same name", false);
            return;
        }

        let template_current_modules = self.current_module.iter()
            .map(|module| Rc::downgrade(module)).collect();

        current_module.templates.push(Rc::new(RefCell::new(Template {
            name: name.source().to_owned(),
            args: args.iter().map(|arg| arg.source().to_owned()).collect(),
            expr,
            current_modules: template_current_modules,
        })))
    }

    fn compile_function(&mut self, name: Token, expr: Expr) {
        let current_module = self.current_module();
        let current_module_borrow = current_module.borrow();

        if current_module_borrow.output_functions.iter().any(|function| *function.borrow().name == *name.source()) {
            self.error_at(*name.start(), "Tried to define multiple density functions with the same name", false);
            return;
        }

        drop(current_module_borrow);
        let expr = self.compile_expr(expr);

        let mut target_path = self.target_dir.to_owned();

        for module in &self.current_module {
            target_path.push(&module.borrow().name);
        }

        target_path.push(name.source());
        target_path.set_extension("json");

        let mut current_module_mut = current_module.borrow_mut();
        current_module_mut.output_functions.push(Rc::new(RefCell::new(OutputFunction {
            name: name.source().to_owned(),
            path: target_path,
            json: expr,
        })));
    }

    fn compile_module(&mut self, name: Token, statements: Vec<Stmt>) {
        let current_module = self.current_module();
        let current_module_borrow = current_module.borrow();

        if current_module_borrow.sub_modules.iter().any(|module| *module.borrow().name == *name.source()) {
            self.error_at(*name.start(), "Tried to define multiple modules with the same name", false);
            return;
        }

        drop(current_module_borrow);

        let module = Module { name: name.source().to_owned(), sub_modules: vec![], templates: vec![], output_functions: vec![] };
        self.current_module.push(Rc::new(RefCell::new(module)));

        for stmt in statements {
            self.compile_statement(stmt);
        }

        let module = self.current_module.pop().expect("Internal compiler error: Missing module");
        self.current_module().borrow_mut().sub_modules.push(module);
    }

    fn compile_include(&mut self, include_path_token: Token) {
        let include_path = PathBuf::from(include_path_token.source());

        if include_path.is_absolute() {
            self.error_at(*include_path_token.start(), "Include path must be relative", false);
        } else {
            let mut path = self.path.to_owned();
            path.pop();
            path.push(&include_path);

            let (mut functions, compiler) = match crate::compile(path, self.target_dir.to_owned()) {
                Ok(Some(result)) => result,
                Ok(None) => {
                    self.error_at(*include_path_token.start(), &format!("Error in included file: \"{}\"", include_path_token.source()), false);
                    return;
                },
                Err(err) => {
                    self.error_at(*include_path_token.start(), &format!("IO error while trying to compile included file: {}", err), false);
                    return;
                }
            };

            let current_module = self.current_module();
            let mut current_module_borrow = current_module.borrow_mut();

            current_module_borrow.output_functions.append(&mut functions);
            current_module_borrow.templates.append(&mut compiler.top_level_module.borrow_mut().templates);
            current_module_borrow.sub_modules.append(&mut compiler.top_level_module.borrow_mut().sub_modules);
        }
    }

    fn compile_import(&mut self, path: Vec<Token>, selector: Option<Vec<Token>>) {
        let mut module_index: isize = self.current_module.len() as isize - 1;
        let mut module = Rc::clone(&self.top_level_module);

        while module_index >= -1 {
            module = Rc::clone(if module_index >= 0 {
                &self.current_module[module_index as usize]
            } else { &self.top_level_module });

            match Self::find_submodule(&module, &path[0]) {
                Some(sub_module) => {
                    module = sub_module;
                    break;
                },
                None => {},
            }

            module_index -= 1;
        }

        if module_index < -1 {
            self.error_at(*path[0].start(), &format!("Unresolved reference to module: '{}'", path[0].source()), false);
            return;
        }

        for name in path.iter().skip(1) {
            match Self::find_submodule(&module, name) {
                Some(sub_module) => {
                    module = sub_module;
                },
                None => {
                    self.error_at(*name.start(), &format!("Unresolved reference to module: '{}'", name.source()), false);
                    return;
                },
            }
        }

        let current_module = self.current_module();
        let mut current_module_borrow = current_module.borrow_mut();

        for template in &module.borrow().templates {
            let template_name = &template.borrow().name;
            let template_arg_count = template.borrow().args.len();
            let mut allow = true;

            if let Some(selector) = &selector {
                allow = selector.iter().any(|name| *template_name == *name.source());
            }

            if allow {
                if current_module_borrow.templates.iter().any(|template| *template.borrow().name == *template_name && template.borrow().args.len() == template_arg_count) {
                    self.error_at(*path.last().expect("Internal compiler error: Empty import path").start(),
                        &format!("Tried to import template with already existing name: '{}'", template_name), false);
                    return;
                }

                current_module_borrow.templates.push(Rc::clone(&template));
            }
        }

        for sub_module in &module.borrow().sub_modules {
            let sub_module_name = &sub_module.borrow().name;
            let mut allow = true;

            if let Some(selector) = &selector {
                allow = selector.iter().any(|name| *sub_module_name == *name.source());
            }

            if allow {
                if current_module_borrow.sub_modules.iter().any(|sub_module| *sub_module.borrow().name == *sub_module_name) {
                    self.error_at(*path.last().expect("Internal compiler error: Empty import path").start(),
                        &format!("Tried to import module with already existing name: '{}'", sub_module_name), false);
                    return;
                }

                current_module_borrow.sub_modules.push(Rc::clone(&sub_module));
            }
        }
    }

    fn find_submodule(module: &Rc<RefCell<Module>>, name: &Token) -> Option<Rc<RefCell<Module>>> {
        for sub_module in &module.borrow().sub_modules {
            if sub_module.borrow().name == *name.source() {
                return Some(Rc::clone(sub_module));
            }
        }

        None
    }

    fn compile_expr(&mut self, expr: Expr) -> JsonElement {
        match expr {
            Expr::ConstantFloat(value) => JsonElement::ConstantFloat(value),
            Expr::ConstantInt(value) => JsonElement::ConstantInt(value),
            Expr::ConstantString(value) => JsonElement::ConstantString(value),
            Expr::Identifier(name) => {
                if let Some((_, element)) = self.template_args.iter().rfind(|(arg, _)| *arg == *name.source()) {
                    element.clone()
                } else {
                    self.error_at(*name.start(), &format!("Unresolved reference: '{}'", name.source()), false);
                    JsonElement::Error
                }
            }
            Expr::Group(expr) => self.compile_expr(*expr),
            Expr::UnaryOperator { operator, expr } => {
                let compiled_expr = self.compile_expr(*expr);

                self.evaluate_template(None, operator, vec![compiled_expr])
            },
            Expr::BinaryOperator { left, operator, right } => {
                let compiled_left = self.compile_expr(*left);
                let compiled_right = self.compile_expr(*right);

                self.evaluate_template(None, operator, vec![compiled_left, compiled_right])
            },
            Expr::FunctionCall { receiver, name, args } => {
                let compiled_receiver = receiver.map(|receiver| self.compile_expr(*receiver));
                let compiled_args = args.into_iter().map(|arg| self.compile_expr(arg)).collect();

                self.evaluate_template(compiled_receiver, name, compiled_args)
            },
            Expr::Member { receiver, name } => {
                let _ = self.compile_expr(*receiver);

                self.error_at(*name.start(), &format!("Unresolved reference: '{}'", name.source()), false);
                JsonElement::Error
            },
            Expr::Object(fields) => {
                JsonElement::Object(fields.into_iter().map(|(name, field)| (name.source().to_owned(), self.compile_expr(field))).collect())
            },
            Expr::Array(elements) => {
                JsonElement::Array(elements.into_iter().map(|element| self.compile_expr(element)).collect())
            },
            Expr::Error => JsonElement::Error,
        }
    }

    fn evaluate_template(&mut self, receiver: Option<JsonElement>, name: Token, args: Vec<JsonElement>) -> JsonElement {
        if let Some(_) = receiver {
            self.error_at(*name.start(), "Member functions are not implemented yet", false);
        }

        let current_module = self.current_module();
        let current_module = current_module.borrow();

        let template = match current_module.templates.iter().find(|template| *template.borrow().name == *name.source() && template.borrow().args.len() == args.len()) {
            Some(template) => template,
            None => {
                self.error_at(*name.start(), &format!("Unresolved function call: {}", name.source()), false);
                return JsonElement::Error;
            },
        };
        let template_borrow = template.borrow();

        let arg_count = template_borrow.args.len();

        for i in 0..arg_count {
            let name = template_borrow.args[i].clone();
            let element = args[i].clone();

            self.template_args.push((name, element));
        }

        let mut template_current_modules: Vec<Rc<RefCell<Module>>> = template_borrow.current_modules.iter()
            .map(|module| module.upgrade().expect("Internal compiler error: Module got dropped")).collect();
        let template_current_modules_count = template_current_modules.len();

        let template_expr = template_borrow.expr.clone();
        drop(template_borrow);
        drop(current_module);

        self.current_module.append(&mut template_current_modules);

        let expr = self.compile_expr(template_expr.clone());
        self.template_args.truncate(self.template_args.len().saturating_sub(arg_count));
        self.current_module.truncate(self.current_module.len().saturating_sub(template_current_modules_count));

        expr
    }

    fn current_module(&self) -> Rc<RefCell<Module>> {
        self.current_module.last().map(|module| Rc::clone(&module)).unwrap_or_else(|| Rc::clone(&self.top_level_module))
    }

    // Error handling

    pub fn had_error(&self) -> bool {
        self.had_error
    }

    fn error_at(&mut self, pos: TokenPos, message: &str, panic: bool) {
        if self.panic_mode {
            return;
        } else if panic {
            self.panic_mode = true;
        }

        eprintln!("[{}:{}:{}] Error: {}", self.path.to_string_lossy(), pos.line, pos.column, message);
        self.had_error = true;
    }
}
