use crate::compiler::ast::{Expr, JsonElement, Stmt};
use crate::compiler::lexer::{Token, TokenPos};

struct Template {
    name: String, // Can be identifiers or operator names (like `+`)
    args: Vec<String>,
    expr: Expr,
}

pub struct Compiler {
    templates: Vec<Template>,
    template_args: Vec<(String, JsonElement)>,

    output_functions: Vec<(String, JsonElement)>,

    had_error: bool, panic_mode: bool,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            templates: vec![],
            template_args: vec![],

            output_functions: vec![],

            had_error: false, panic_mode: false,
        }
    }

    pub fn compile(mut self, statements: Vec<Stmt>) -> (Vec<(String, JsonElement)>, bool) {
        for stmt in statements {
            self.compile_statement(stmt);
        }

        (self.output_functions, self.had_error)
    }

    fn compile_statement(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Template { name, args, expr } => self.compile_template(name, args, expr),
            Stmt::Function { name, expr } => self.compile_function(name, expr),
            Stmt::Error => {},
        }
    }

    fn compile_template(&mut self, name: Token, args: Vec<Token>, expr: Expr) {
        if self.templates.iter().any(|template| *template.name == *name.source() && template.args.len() == args.len()) {
            self.error_at(Some(*name.start()), "Tried to define multiple templates with the same name", false);
            return;
        }

        self.templates.push(Template {
            name: name.source().to_owned(),
            args: args.iter().map(|arg| arg.source().to_owned()).collect(),
            expr
        })
    }

    fn compile_function(&mut self, name: Token, expr: Expr) {
        if self.output_functions.iter().any(|(function_name, _)| *function_name == *name.source()) {
            self.error_at(Some(*name.start()), "Tried to define multiple density functions with the same name", false);
            return;
        }

        let expr = self.compile_expr(expr);
        self.output_functions.push((name.source().to_owned(), expr));
    }

    fn compile_expr(&mut self, expr: Expr) -> JsonElement {
        match expr {
            Expr::ConstantFloat(value) => JsonElement::ConstantFloat(value),
            Expr::ConstantInt(value) => JsonElement::ConstantInt(value),
            Expr::ConstantString(value) => JsonElement::ConstantString(value),
            Expr::Identifier(id) => {
                if let Some((_, element)) = self.template_args.iter().rfind(|(name, _)| *name == *id.source()) {
                    element.clone()
                } else {
                    self.error_at(Some(*id.start()), &format!("Unresolved reference: {}", id.source()), false);
                    JsonElement::ConstantString(id.source().to_owned())
                }
            },
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
            Expr::Member { name, .. } => {
                self.error_at(Some(*name.start()), &format!("Unresolved reference: {}", name.source()), false);
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
            self.error_at(Some(*name.start()), "Member functions are not implemented yet", false);
        }

        let template = match self.templates.iter().find(|template| *template.name == *name.source() && template.args.len() == args.len()) {
            Some(template) => template,
            None => {
                self.error_at(Some(*name.start()), &format!("Unresolved function call: {}", name.source()), false);
                return JsonElement::Error;
            },
        };

        let arg_count = template.args.len();

        for i in 0..arg_count {
            let name = template.args[i].clone();
            let element = args[i].clone();

            self.template_args.push((name, element));
        }

        let expr = self.compile_expr(template.expr.clone());
        self.template_args.truncate(self.template_args.len().saturating_sub(arg_count));

        expr
    }

    pub fn had_error(&self) -> bool {
        self.had_error
    }

    fn error_at(&mut self, pos: Option<TokenPos>, message: &str, panic: bool) {
        Self::error_at_impl(&mut self.had_error, &mut self.panic_mode, pos, message, panic);
    }

    fn error_at_impl(had_error: &mut bool, panic_mode: &mut bool, pos: Option<TokenPos>, message: &str, panic: bool) {
        if *panic_mode {
            return;
        } else if panic {
            *panic_mode = true;
        }

        if let Some(pos) = pos {
            eprint!("{} Error: ", pos);
        } else {
            eprint!("Error: ");
        }

        eprintln!("{}", message);
        *had_error = true;
    }
}
