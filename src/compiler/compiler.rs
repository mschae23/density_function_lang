use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::{Expr, JsonElement, Module, ExportFunction, Decl, Template, TemplateExpr, JsonElementType};
use crate::compiler::lexer::{Token, TokenPos};

struct TemplateScope {
    pub args: Vec<(String, JsonElement)>,
}

struct TemplateCall {
    pub name: String,
    pub receiver: bool,
    pub arg_count: i32,

    pub path: Rc<RefCell<PathBuf>>,
    pub line: i32,
    pub column: i32,
}

pub struct Compiler {
    current_module: Vec<Rc<RefCell<Module>>>,
    top_level_module: Rc<RefCell<Module>>,
    template_scopes: Vec<TemplateScope>,
    template_call_stack: Vec<TemplateCall>,

    path: Rc<RefCell<PathBuf>>, target_dir: PathBuf,

    had_error: bool, panic_mode: bool,
    verbose: bool,
}

impl Compiler {
    pub fn new(path: PathBuf, target_dir: PathBuf, verbose: bool) -> Compiler {
        Compiler {
            current_module: vec![],
            top_level_module: Rc::new(RefCell::new(Module { name: String::from("<top-level>"),
                sub_modules: vec![], templates: vec![], exports: vec![] })),
            template_scopes: vec![],
            template_call_stack: vec![],

            path: Rc::new(RefCell::new(path)), target_dir,

            had_error: false, panic_mode: false,
            verbose,
        }
    }

    pub fn compile(&mut self, statements: Vec<Decl>) -> Vec<Rc<RefCell<ExportFunction>>> {
        for stmt in statements {
            self.compile_statement(stmt);
        }

        let mut outputs = vec![];
        Self::collect_exports(&mut outputs, Rc::clone(&self.top_level_module));
        outputs
    }

    fn collect_exports(outputs: &mut Vec<Rc<RefCell<ExportFunction>>>, module: Rc<RefCell<Module>>) {
        outputs.append(&mut module.borrow_mut().exports);

        let module_borrow = module.borrow_mut();

        for sub_module in &module_borrow.sub_modules {
            Self::collect_exports(outputs, Rc::clone(sub_module));
        }
    }

    fn compile_statement(&mut self, stmt: Decl) {
        match stmt {
            Decl::Template { name, this, args, expr } => self.compile_template(name, this, args, expr),
            Decl::Export { name, expr } => self.compile_export(name, expr),
            Decl::Module { name, statements } => self.compile_module(name, statements),
            Decl::Include { path } => self.compile_include(path),
            Decl::Import { path, selector } => self.compile_import(path, selector),
            Decl::Error => {},
        }
    }

    fn compile_template(&mut self, name: Token, this: Option<Token>, args: Vec<Token>, expr: TemplateExpr) {
        let current_module = self.current_module();
        let mut current_module = current_module.borrow_mut();

        if current_module.templates.iter().any(|template|
            *template.borrow().name == *name.source() && template.borrow().args.len() == args.len() && template.borrow().receiver == this.is_some()) {
            self.error_at(*name.start(), "Tried to define multiple templates with the same name", false);
            return;
        }

        let template_current_modules = self.current_module.iter()
            .map(|module| Rc::downgrade(module)).collect();

        current_module.templates.push(Rc::new(RefCell::new(Template {
            name: name.source().to_owned(),
            receiver: this.is_some(),
            args: args.iter().map(|arg| arg.source().to_owned()).collect(),
            expr,
            current_modules: template_current_modules,
            file_path: Rc::clone(&self.path),
        })))
    }

    fn compile_export(&mut self, name: Token, expr: Expr) {
        let current_module = self.current_module();
        let current_module_borrow = current_module.borrow();

        if current_module_borrow.exports.iter().any(|function| *function.borrow().name == *name.source()) {
            self.error_at(*name.start(), "Tried to define multiple exports with the same name", false);
            return;
        }

        drop(current_module_borrow);
        let expr = self.compile_expr(expr, false);

        let mut target_path = self.target_dir.to_owned();

        for module in &self.current_module {
            target_path.push(&module.borrow().name);
        }

        target_path.push(name.source());
        target_path.set_extension("json");

        let mut current_module_mut = current_module.borrow_mut();
        current_module_mut.exports.push(Rc::new(RefCell::new(ExportFunction {
            name: name.source().to_owned(),
            path: target_path,
            json: expr,
        })));
    }

    fn compile_module(&mut self, name: Token, statements: Vec<Decl>) {
        let current_module = self.current_module();
        let current_module_borrow = current_module.borrow();

        if current_module_borrow.sub_modules.iter().any(|module| *module.borrow().name == *name.source()) {
            self.error_at(*name.start(), "Tried to define multiple modules with the same name", false);
            return;
        }

        drop(current_module_borrow);

        let module = Module { name: name.source().to_owned(), sub_modules: vec![], templates: vec![], exports: vec![] };
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
            let mut path = self.path.borrow().to_owned();
            path.pop();
            path.push(&include_path);

            let (mut functions, compiler) = match crate::compile(path, self.target_dir.to_owned(), self.verbose) {
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

            current_module_borrow.exports.append(&mut functions);
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

    fn compile_expr(&mut self, expr: Expr, static_expr: bool) -> JsonElement {
        match expr {
            Expr::ConstantFloat(value) => JsonElement::ConstantFloat(value),
            Expr::ConstantInt(value) => JsonElement::ConstantInt(value),
            Expr::ConstantBoolean(value) => JsonElement::ConstantBoolean(value),
            Expr::ConstantString(value) => JsonElement::ConstantString(value),
            Expr::Identifier(name) => {
                if let Some((_, element)) = self.template_scopes.last().map(|scope| scope.args.iter().rfind(|(arg, _)| *arg == *name.source())).flatten() {
                    element.clone()
                } else {
                    self.evaluate_identifier(name)
                }
            }
            // Expr::Group(expr) => self.compile_expr(*expr),
            Expr::UnaryOperator { operator, expr } => {
                let compiled_expr = self.compile_expr(*expr, static_expr);

                let pos = *operator.start();
                self.evaluate_template(Expr::Identifier(operator), pos, vec![compiled_expr], static_expr)
            },
            Expr::BinaryOperator { left, operator, right } => {
                let compiled_left = self.compile_expr(*left, static_expr);
                let compiled_right = self.compile_expr(*right, static_expr);

                let pos = *operator.start();
                self.evaluate_template(Expr::Identifier(operator), pos, vec![compiled_left, compiled_right], static_expr)
            },
            Expr::FunctionCall { callee, token: paren_left, args } => {
                let compiled_args = args.into_iter().map(|arg| self.compile_expr(arg, static_expr)).collect();

                self.evaluate_template(*callee, *paren_left.start(), compiled_args, static_expr)
            },
            Expr::Member { receiver, name } => {
                let compiled_receiver = self.compile_expr(*receiver, static_expr);
                self.evaluate_member(compiled_receiver, name, static_expr)
            },
            Expr::BuiltinFunctionCall { name, args } => {
                self.evaluate_builtin_call(name, args, static_expr)
            },
            Expr::BuiltinType(element_type) => JsonElement::Type(element_type),
            Expr::Object(fields) => {
                JsonElement::Object(fields.into_iter().map(|(name, field)| (name.source().to_owned(), self.compile_expr(field, static_expr))).collect())
            },
            Expr::Array(elements) => {
                JsonElement::Array(elements.into_iter().map(|element| self.compile_expr(element, static_expr)).collect())
            },
            Expr::Error => JsonElement::Error,
        }
    }

    fn evaluate_template(&mut self, callee: Expr, token_pos: TokenPos, args: Vec<JsonElement>, static_expr: bool) -> JsonElement {
        let (receiver, name) = match callee {
            Expr::Member { receiver, name} => (Some(self.compile_expr(*receiver, static_expr)), name),
            Expr::Identifier(name) => (None, name),
            _ => {
                self.error_at(token_pos, "Cannot call non-identifier expression", true);
                return JsonElement::Error;
            },
        };

        if static_expr {
            if let Some(result) = self.evaluate_static_operator(&name, &args) {
                return result;
            }
        }

        let template = match self.find_template(&name, receiver.as_ref(), args.len()) {
            Some(template) => template,
            None => {
                self.error_at(*name.start(), &format!("Unresolved function call: {}", name.source()), false);
                return JsonElement::Error;
            }
        };
        let template_borrow = template.borrow();

        let mut scope_args = vec![];
        // eprintln!("name: {}; template receiver: {}; provided receiver: {}; template args: {:?}; provided args: {:?}",
        //     &template_borrow.name, template_borrow.receiver, receiver.is_some(), &template_borrow.args, &args);

        if let Some(receiver) = receiver {
            scope_args.push((String::from("this"), receiver));
        }

        for i in 0..template_borrow.args.len() {
            let name = template_borrow.args[i].clone();
            let element = args[i].clone();

            scope_args.push((name, element));
        }

        self.template_scopes.push(TemplateScope { args: scope_args });
        self.template_call_stack.push(TemplateCall {
            name: template_borrow.name.to_owned(),
            receiver: template_borrow.receiver,
            arg_count: template_borrow.args.len() as i32,
            path: Rc::clone(&self.path),
            line: token_pos.line, column: token_pos.column,
        });

        let mut template_current_modules: Vec<Rc<RefCell<Module>>> = template_borrow.current_modules.iter()
            .map(|module| module.upgrade().expect("Internal compiler error: Module got dropped")).collect();

        let template_expr = template_borrow.expr.clone();
        let mut template_file_path = Rc::clone(&template_borrow.file_path);
        drop(template_borrow);

        std::mem::swap(&mut self.current_module, &mut template_current_modules);
        std::mem::swap(&mut self.path, &mut template_file_path);

        let expr = self.compile_template_expr(template_expr.clone(), static_expr);
        self.template_call_stack.pop();
        self.template_scopes.pop();
        std::mem::swap(&mut self.current_module, &mut template_current_modules);
        std::mem::swap(&mut self.path, &mut template_file_path);

        expr
    }

    fn evaluate_static_operator(&mut self, name: &Token, args: &Vec<JsonElement>) -> Option<JsonElement> {
        macro_rules! make_expr {
            ($x:expr) => { $x }
        }

        macro_rules! binary_number_op {
            ($args:expr, $op:tt) => { match &$args[0] {
                JsonElement::ConstantFloat(left) => match &$args[1] {
                    JsonElement::ConstantFloat(right) => return Some(JsonElement::ConstantFloat(make_expr!(*left $op *right))),
                    JsonElement::ConstantInt(right) => return Some(JsonElement::ConstantFloat(make_expr!(*left $op (*right as f64)))),
                    element => self.error_at_static_operator_arg_type(name, element,
                        &[JsonElementType::Float, JsonElementType::Int]),
                },
                JsonElement::ConstantInt(left) => match &$args[1] {
                    JsonElement::ConstantFloat(right) => return Some(JsonElement::ConstantFloat(make_expr!((*left as f64) $op *right))),
                    JsonElement::ConstantInt(right) => return Some(JsonElement::ConstantInt(make_expr!(*left $op *right))),
                    element => self.error_at_static_operator_arg_type(name, element,
                        &[JsonElementType::Float, JsonElementType::Int]),
                },
                element => self.error_at_static_operator_arg_type(name, element,
                    &[JsonElementType::Float, JsonElementType::Int]),
            } }
        }

        macro_rules! binary_compare_op {
            ($args:expr, $op:tt) => { match &$args[0] {
                JsonElement::ConstantFloat(left) => match &$args[1] {
                    JsonElement::ConstantFloat(right) => return Some(JsonElement::ConstantBoolean(make_expr!(*left $op *right))),
                    JsonElement::ConstantInt(right) => return Some(JsonElement::ConstantBoolean(make_expr!(*left $op (*right as f64)))),
                    element => self.error_at_static_operator_arg_type(name, element,
                        &[JsonElementType::Float, JsonElementType::Int]),
                },
                JsonElement::ConstantInt(left) => match &$args[1] {
                    JsonElement::ConstantFloat(right) => return Some(JsonElement::ConstantBoolean(make_expr!((*left as f64) $op *right))),
                    JsonElement::ConstantInt(right) => return Some(JsonElement::ConstantBoolean(make_expr!(*left $op *right))),
                    element => self.error_at_static_operator_arg_type(name, element,
                        &[JsonElementType::Float, JsonElementType::Int]),
                },
                element => self.error_at_static_operator_arg_type(name, element,
                    &[JsonElementType::Float, JsonElementType::Int]),
            } }
        }

        macro_rules! binary_logic_op {
            ($args:expr, $op:tt) => { match &$args[0] {
                JsonElement::ConstantBoolean(left) => match &$args[1] {
                    JsonElement::ConstantBoolean(right) => return Some(JsonElement::ConstantBoolean(make_expr!(*left $op *right))),
                    element => self.error_at_static_operator_arg_type(name, element,
                        &[JsonElementType::Boolean]),
                },
                element => self.error_at_static_operator_arg_type(name, element,
                    &[JsonElementType::Boolean]),
            } }
        }

        if args.len() == 1 {
            match name.source() {
                "+" => match &args[0] {
                    element @ JsonElement::ConstantFloat(_)
                        | element @ JsonElement::ConstantInt(_) => return Some(element.clone()),
                    element => self.error_at_static_operator_arg_type(name, element,
                        &[JsonElementType::Float, JsonElementType::Int])
                },
                "-" => match &args[0] {
                    JsonElement::ConstantFloat(value) => return Some(JsonElement::ConstantFloat(-(*value))),
                    JsonElement::ConstantInt(value) => return Some(JsonElement::ConstantInt(-(*value))),
                    element => self.error_at_static_operator_arg_type(name, element,
                        &[JsonElementType::Float, JsonElementType::Int])
                },
                "!" => match &args[0] {
                    JsonElement::ConstantBoolean(value) => return Some(JsonElement::ConstantBoolean(!(*value))),
                    element => self.error_at_static_operator_arg_type(name, element,
                        &[JsonElementType::Boolean])
                },
                _ => {},
            }
        } else if args.len() == 2 {
            match name.source() {
                "+" => binary_number_op!(args, +),
                "-" => binary_number_op!(args, -),
                "*" => binary_number_op!(args, *),
                "/" => binary_number_op!(args, /),

                "<" => binary_compare_op!(args, <),
                "<=" => binary_compare_op!(args, <=),
                ">" => binary_compare_op!(args, >),
                ">=" => binary_compare_op!(args, >=),

                "==" => return Some(JsonElement::ConstantBoolean(args[0] == args[1])),
                "!=" => return Some(JsonElement::ConstantBoolean(args[0] != args[1])),

                "&&" => binary_logic_op!(args, &&),
                "||" => binary_logic_op!(args, ||),
                _ => {},
            }
        }

        None
    }

    fn error_at_static_operator_arg_type(&mut self, operator: &Token, element: &JsonElement, types: &[JsonElementType]) {
        self.error_at_with_context(*operator.start(), "Argument to static operator has wrong type", vec![
            ("Expected", JsonElement::Array(types.iter().map(|element_type| JsonElement::Type(*element_type)).collect())),
            ("Actual", JsonElement::Type(JsonElementType::from(element)))
        ], false);
    }

    fn compile_template_expr(&mut self, expr: TemplateExpr, static_expr: bool) -> JsonElement {
        match expr {
            TemplateExpr::Block { expressions, last } => {
                for expr in expressions {
                    self.compile_template_expr(expr, static_expr);
                }

                self.compile_template_expr(*last, static_expr)
            },
            TemplateExpr::If { token, condition, then, otherwise } => {
                let compiled_condition = self.compile_expr(condition, true);

                match compiled_condition {
                    JsonElement::ConstantBoolean(value) =>
                        self.compile_template_expr(if value { *then } else { *otherwise }, static_expr),
                    element => {
                        self.error_at_with_context(*token.end(), "Condition of 'if' expression must be a boolean value",
                            vec![("Context", element)], true);
                        JsonElement::Error
                    },
                }
            },
            TemplateExpr::Simple(expr) => self.compile_expr(expr, static_expr),
        }
    }

    fn find_template(&mut self, name: &Token, receiver: Option<&JsonElement>, arg_count: usize) -> Option<Rc<RefCell<Template>>> {
        if let Some(receiver) = receiver {
            match receiver {
                JsonElement::Module(module) => return Self::find_template_on(module, name, false, arg_count),
                _ => {},
            }
        }

        let mut module_index: isize = self.current_module.len() as isize - 1;

        while module_index >= -1 {
            let module = Rc::clone(if module_index >= 0 {
                &self.current_module[module_index as usize]
            } else { &self.top_level_module });

            if let Some(template) = Self::find_template_on(&module, &name, receiver.is_some(), arg_count) {
                return Some(template);
            }

            module_index -= 1;
        }

        None
    }

    fn find_template_on(module: &Rc<RefCell<Module>>, name: &Token, receiver: bool, arg_count: usize) -> Option<Rc<RefCell<Template>>> {
        for template in &module.borrow().templates {
            if *template.borrow().name == *name.source() && template.borrow().args.len() == arg_count && (template.borrow().receiver == receiver) {
                return Some(Rc::clone(template));
            }
        }

        None
    }

    fn evaluate_builtin_call(&mut self, name: Token, args: Vec<Expr>, static_expr: bool) -> JsonElement {
        if "error" == name.source() {
            let args = args.into_iter().map(|arg| self.compile_expr(arg, static_expr)).collect();
            return self.evaluate_builtin_error_call(name, args);
        } else if "static" == name.source() {
            if args.len() != 1 {
                self.error_at(*name.start(), "builtin.static() needs exactly one argument", true);
                return JsonElement::Error;
            }

            return self.compile_expr(args.into_iter()
                .next().expect("Internal compiler error: builtin.static() argument missing"), true);
        }

        self.error_at(*name.start(), &format!("Unknown built-in function: '{}'", name.source()), false);
        JsonElement::Error
    }

    fn evaluate_builtin_error_call(&mut self, name: Token, args: Vec<JsonElement>) -> JsonElement {
        if args.len() == 0 {
            self.error_at(*name.start(), "builtin.error() called", false);
        } else if args.len() > 0 {
            match &args[0] {
                JsonElement::ConstantString(msg) =>
                    self.error_at_with_context(*name.start(), &msg.to_owned(), args.into_iter()
                        .skip(1).map(|arg| ("Context", arg)).collect(), false),
                _ => self.error_at_with_context(*name.start(), "builtin.error() called", args.into_iter()
                    .map(|arg| ("Context", arg)).collect(), false),
            }
        }

        JsonElement::Error
    }

    fn evaluate_identifier(&mut self, name: Token) -> JsonElement {
        let mut module_index: isize = self.current_module.len() as isize - 1;

        while module_index >= -1 {
            let module = Rc::clone(if module_index >= 0 {
                &self.current_module[module_index as usize]
            } else { &self.top_level_module });

            for template in &module.borrow().templates {
                if *template.borrow().name == *name.source() && !template.borrow().receiver {
                    return JsonElement::Template(Rc::clone(template));
                }
            }

            for sub_module in &module.borrow().sub_modules {
                if *sub_module.borrow().name == *name.source() {
                    return JsonElement::Module(Rc::clone(sub_module));
                }
            }

            module_index -= 1;
        }

        self.error_at(*name.start(), &format!("Unresolved reference: '{}'", name.source()), false);
        JsonElement::Error
    }

    fn evaluate_member(&mut self, receiver: JsonElement, name: Token, static_expr: bool) -> JsonElement {
        match receiver {
            JsonElement::Module(module) => {
                for template in &module.borrow().templates {
                    if *template.borrow().name == *name.source() {
                        return JsonElement::Template(Rc::clone(template));
                    }
                }

                for sub_module in &module.borrow().sub_modules {
                    if *sub_module.borrow().name == *name.source() {
                        return JsonElement::Module(Rc::clone(sub_module));
                    }
                }

                self.error_at(*name.start(), &format!("Unresolved reference: '{}'", name.source()), false);
                JsonElement::Error
            },
            receiver => {
                if static_expr && name.source() == "type" {
                    return JsonElement::Type(JsonElementType::from(receiver));
                }

                self.error_at(*name.start(), "Tried to get a member of non-module value", true);
                JsonElement::Error
            },
        }
    }

    fn current_module(&self) -> Rc<RefCell<Module>> {
        self.current_module.last().map(|module| Rc::clone(&module)).unwrap_or_else(|| Rc::clone(&self.top_level_module))
    }

    // Error handling

    pub fn had_error(&self) -> bool {
        self.had_error
    }

    fn error_at(&mut self, pos: TokenPos, message: &str, panic: bool) {
        self.error_at_with_context(pos, message, Vec::new(), panic)
    }

    fn error_at_with_context(&mut self, pos: TokenPos, message: &str, context: Vec<(&str, JsonElement)>, panic: bool) {
        if self.panic_mode {
            return;
        } else if panic {
            self.panic_mode = true;
        }

        eprintln!("[{}:{}:{}] Error: {}", self.path.borrow().to_string_lossy(), pos.line, pos.column, message);

        for (name, context_element) in &context {
            eprintln!("    {}: {:?}", *name, context_element);
        }

        if self.verbose && !context.is_empty() && !self.template_call_stack.is_empty() {
            eprintln!();
        }

        if self.verbose {
            for template_call in self.template_call_stack.iter().rev() {
                let mut args = vec![];

                if template_call.receiver {
                    args.push(String::from("this"));
                }

                if template_call.arg_count > 0 {
                    args.push(template_call.arg_count.to_string());
                }

                eprintln!("    at [{}:{}:{}] {}({})", template_call.path.borrow().to_string_lossy(),
                    template_call.line, template_call.column, &template_call.name, args.join(", "));
            }
        }

        self.had_error = true;
    }
}
