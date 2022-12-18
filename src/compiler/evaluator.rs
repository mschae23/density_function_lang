use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::{Rc, Weak};
use crate::compiler::ast::simple::{Expr, JsonElement, Module, ExportFunction, Decl, Template, TemplateExpr, JsonElementType, CompiledExport};
use crate::compiler::ast::typed::{TypedDecl, TypedTemplateDecl, TypedVariableDecl};
use crate::compiler::environment::RecursionToParent;
use crate::compiler::lexer::{Token, TokenPos};
use crate::Config;

#[derive(Debug)]
pub struct DeclarationEnvironment {
    templates: Vec<Rc<RefCell<TypedTemplateDecl>>>,
    variables: HashMap<String, Rc<RefCell<TypedVariableDecl>>>,
    modules: HashMap<String, Rc<RefCell<DeclarationEnvironment>>>,
    imports: Option<Rc<RefCell<DeclarationEnvironment>>>,
    parent: Option<Weak<RefCell<DeclarationEnvironment>>>,
}

impl DeclarationEnvironment {
    pub fn new() -> Self {
        DeclarationEnvironment {
            templates: Vec::new(),
            variables: HashMap::new(),
            modules: HashMap::new(),
            imports: None,
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Rc<RefCell<DeclarationEnvironment>>) -> Rc<RefCell<DeclarationEnvironment>> {
        Rc::new(RefCell::new(DeclarationEnvironment {
            templates: Vec::new(),
            modules: HashMap::new(),
            variables: HashMap::new(),
            imports: None,
            parent: Some(Rc::downgrade(&parent)),
        }))
    }

    pub fn get_or_create_import_environment(this: Rc<RefCell<DeclarationEnvironment>>) -> Rc<RefCell<DeclarationEnvironment>> {
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

    // These functions don't check if the items already exist;
    // it is assumed that this was already done by the type checker
    pub fn add_template(&mut self, template: Rc<RefCell<TypedTemplateDecl>>) {
        self.templates.push(template);
    }

    pub fn add_variable(&mut self, variable: Rc<RefCell<TypedVariableDecl>>) {
        self.variables.insert(variable.borrow().name.source().to_owned(), variable);
    }

    pub fn add_module(&mut self, name: String, module: Rc<RefCell<DeclarationEnvironment>>) {
        self.modules.insert(name, module);
    }

    pub fn find_module(&self, name: &str, recursion: RecursionToParent) -> Option<Rc<RefCell<DeclarationEnvironment>>> {
        self.modules.get(name).map(Rc::clone)
            .or_else(|| if recursion.should_recurse() {
                self.imports.as_ref().and_then(|imports| imports.borrow().find_module(name, RecursionToParent::Never))
            } else { None })
            .or_else(|| if recursion.should_recurse() {
                self.parent.as_ref().and_then(Weak::upgrade).and_then(|parent| parent.borrow().find_module(name, recursion.decrement()))
            } else { None })
    }
}

pub struct Evaluator {
    path: PathBuf, target_dir: PathBuf,

    environment: Rc<RefCell<DeclarationEnvironment>>,

    had_error: bool, panic_mode: bool,
    config: Rc<Config>,
}

impl Evaluator {
    pub fn new(path: PathBuf, target_dir: PathBuf, config: Rc<Config>) -> Evaluator {
        Evaluator {
            path, target_dir,
            environment: Rc::new(RefCell::new(DeclarationEnvironment::new())),
            had_error: false, panic_mode: false,
            config,
        }
    }

    pub fn evaluate_declarations(&mut self, declarations: Vec<TypedDecl>) {
        for decl in &declarations {
            self.declare_declaration(decl);
        }

        for decl in declarations {
            self.evaluate_decl(decl);
        }
    }

    fn declare_declaration(&mut self, decl: &TypedDecl) {
        match decl {
            TypedDecl::Template(template) => {
                self.environment.borrow_mut().add_template(Rc::clone(&template));
            },
            TypedDecl::Variable(variable) => {
                self.environment.borrow_mut().add_variable(Rc::clone(&variable));
            },
            TypedDecl::Import { path, selector } => {
                let mut environment = self.environment();

                for name in &path {
                    let result = environment.borrow().find_module(name.source(), RecursionToParent::Always);

                    match result {
                        Some(result) => environment = result,
                        None => return // error,
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
            TypedDecl::Module(module) => {
                let current_environment = Rc::clone(&self.environment);
                let child_environment = DeclarationEnvironment::new_with_parent(Rc::clone(&current_environment));
                self.environment = Rc::clone(&child_environment);

                for decl in &module.borrow().declarations {
                    self.declare_declaration(decl);
                }

                current_environment.borrow_mut().add_module(name.source().to_owned(), Rc::clone(&child_environment));
                self.environment = current_environment;
            },
            _ => {},
        }
    }

    fn evaluate_decl(&mut self, decl: TypedDecl) {
        match decl {
            TypedDecl::Template(_) => {},
            TypedDecl::Variable(_) => {},
            TypedDecl::Module(_) => {},
            TypedDecl::Include { .. } => {},
            TypedDecl::Import { .. } => {},
            TypedDecl::Error => {},
        }
    }

    fn environment(&self) -> Rc<RefCell<DeclarationEnvironment>> {
        Rc::clone(&self.environment)
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

        eprintln!("[{}:{}:{}] Error: {}", self.path.to_string_lossy(), pos.line, pos.column, message);

        for (name, context_element) in &context {
            eprintln!("    {}: {:?}", *name, context_element);
        }

        /* if self.config.verbose && !context.is_empty() && !self.template_call_stack.is_empty() {
            eprintln!();
        }

        if self.config.verbose {
            for template_call in self.template_call_stack.iter().rev() {
                let mut args = vec![];

                if template_call.receiver {
                    args.push(String::from("this"));
                }

                if template_call.arg_count > 0 {
                    args.push(template_call.arg_count.to_string());
                }

                let template = template_call.template.borrow();
                let template_path = template.current_modules.iter()
                    .map(|module| module.upgrade()).map(|module| module
                        .map(|module| module.borrow().name.to_owned()))
                    .fold(Some(String::from("")), |acc, module| if let Some(mut acc) = acc {
                        if let Some(module) = module {
                            acc.push_str(&module);
                            acc.push('.');
                            Some(acc)
                        } else { None }
                    } else { None }).unwrap_or_default();

                if template_call.tailrec_index > 0 {
                    if template_call.tailrec_index == 1 {
                        eprintln!("    ... 1 tail recursion call");
                    } else {
                        eprintln!("    ... {} tail recursion calls", template_call.tailrec_index);
                    }
                }

                eprintln!("    at [{}:{}:{}] {}{}({})", template_call.path.borrow().to_string_lossy(),
                    template_call.line, template_call.column, template_path, &template_call.name, args.join(", "));
            }
        } */

        self.had_error = true;
    }
}
