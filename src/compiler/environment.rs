use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use crate::compiler::ast::typed::{ExprType, ModuleDeclaration, TemplateDeclaration, VariableDeclaration};
use crate::compiler::lexer::Token;
use crate::compiler::type_checker::hint::{TemplateType, TypeHint};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RecursionToParent {
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
pub struct TypeEnvironment {
    pub templates: Vec<Rc<RefCell<TemplateDeclaration>>>,
    pub modules: HashMap<String, Rc<RefCell<ModuleDeclaration>>>,
    pub variables: HashMap<String, Rc<RefCell<VariableDeclaration>>>,

    parent: Option<Weak<RefCell<TypeEnvironment>>>,
    imports: Option<Rc<RefCell<TypeEnvironment>>>,
    pub children: HashMap<String, Rc<RefCell<TypeEnvironment>>>,
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

    pub fn has_module(&self, name: &str, recursion: RecursionToParent) -> bool {
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