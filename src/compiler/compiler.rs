use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use crate::compiler::ast::{Expr, JsonElement, Module, ExportFunction, Decl, Template, TemplateExpr, JsonElementType, CompiledExport};
use crate::compiler::lexer::{Token, TokenPos};
use crate::Config;

struct TemplateScope {
    pub args: Vec<(String, JsonElement)>,
}

struct TemplateCall {
    pub template: Rc<RefCell<Template>>,
    pub name: String,
    pub receiver: bool,
    pub arg_count: i32,

    pub path: Rc<RefCell<PathBuf>>,
    pub line: i32,
    pub column: i32,

    pub tailrec_index: u32,
}

type TemplateCompileData = (TemplateExpr, Vec<Rc<RefCell<Module>>>, Rc<RefCell<PathBuf>>);

pub struct Compiler {
    had_error: bool, panic_mode: bool,
    config: Rc<Config>,
}

impl Compiler {
    pub fn new(path: PathBuf, target_dir: PathBuf, config: Rc<Config>) -> Compiler {
        Compiler {
            had_error: false, panic_mode: false,
            config,
        }
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

        if self.config.verbose && !context.is_empty() && !self.template_call_stack.is_empty() {
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
        }

        self.had_error = true;
    }
}
