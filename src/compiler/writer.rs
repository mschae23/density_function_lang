use std::io::Write;
use crate::compiler::ast::JsonElement;

pub struct JsonWriter {
    indentation: String,
    pretty: bool,

    indent_level: usize,
}

impl JsonWriter {
    pub fn new(indentation: String, pretty: bool) -> JsonWriter {
        JsonWriter {
            indentation, pretty,
            indent_level: 0,
        }
    }

    pub fn write_element(&mut self, element: &JsonElement, out: &mut impl Write) -> Result<(), std::io::Error> {
        match element {
            JsonElement::ConstantFloat(value) =>
                if *value as i32 as f64 == *value { write!(out, "{:.1}", value) }
                else if value.abs() > 1_000_000_000_000_f64 || value.abs() < 0.000_000_1 { write!(out, "{:E}", value) }
                else { write!(out, "{:.}", value) },
            JsonElement::ConstantInt(value) => write!(out, "{}", value),
            JsonElement::ConstantBoolean(value) => write!(out, "{}", value),
            JsonElement::ConstantString(value) => write!(out, "\"{}\"", value),

            JsonElement::Object(fields) => self.write_object(fields, out),
            JsonElement::Array(elements) => self.write_array(elements, out),

            JsonElement::Module(_) | JsonElement::Template(_) => write!(out, "Error"),
            JsonElement::Type(_) => write!(out, "Error"),
            JsonElement::Error => write!(out, "null"),
        }
    }

    fn write_object(&mut self, fields: &Vec<(String, JsonElement)>, out: &mut impl Write) -> Result<(), std::io::Error> {
        write!(out, "{{")?;

        if self.pretty {
            write!(out, "\n")?;
            self.indent_level += 1;

            let indentation = self.indentation.repeat(self.indent_level);
            let field_count = fields.len();

            for (i, (name, field)) in fields.into_iter().enumerate() {
                write!(out, "{}\"{}\": ", &indentation, name)?;
                self.write_element(field, out)?;

                if i < field_count - 1 {
                    write!(out, ",")?;
                }

                write!(out, "\n")?;
            }

            self.indent_level -= 1;
            write!(out, "{}}}", self.indentation.repeat(self.indent_level))?;
        } else {
            let field_count = fields.len();

            for (i, (name, field)) in fields.into_iter().enumerate() {
                write!(out, "\"{}\": ", name)?;
                self.write_element(field, out)?;

                if i < field_count - 1 {
                    write!(out, ", ")?;
                }
            }

            write!(out, "}}")?;
        }

        Ok(())
    }

    fn write_array(&mut self, elements: &Vec<JsonElement>, out: &mut impl Write) -> Result<(), std::io::Error> {
        write!(out, "[")?;

        if self.pretty {
            write!(out, "\n")?;
            self.indent_level += 1;

            let indentation = self.indentation.repeat(self.indent_level);
            let field_count = elements.len();

            for (i, element) in elements.into_iter().enumerate() {
                write!(out, "{}", &indentation)?;
                self.write_element(element, out)?;

                if i < field_count - 1 {
                    write!(out, ",")?;
                }

                write!(out, "\n")?;
            }

            self.indent_level -= 1;
            write!(out, "{}]", self.indentation.repeat(self.indent_level))?;
        } else {
            let field_count = elements.len();

            for (i, element) in elements.into_iter().enumerate() {
                self.write_element(element, out)?;

                if i < field_count - 1 {
                    write!(out, ", ")?;
                }
            }

            write!(out, "]")?;
        }

        Ok(())
    }
}
