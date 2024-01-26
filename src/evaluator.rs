use std::collections::HashMap;

use crate::parser::{Branch, Constructor, Expr, Pattern, TopLevel};

pub struct Evaluator {
    ctx: HashMap<String, Value>,
    locals: Vec<(String, Value)>,

    stack: Vec<Value>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            ctx: HashMap::new(),
            locals: vec![],
            stack: vec![],
        }
    }

    fn resolve_word(&self, word: &str) -> Value {
        match self.locals.iter().rev().find(|(name, _)| name == word) {
            Some((_, ty)) => ty.clone(),
            None => self.ctx.get(word).unwrap().clone(),
        }
    }

    fn fits_pattern(&self, value: &Value, pattern: &Pattern) -> bool {
        match (value, pattern) {
            (
                Value::Basic {
                    constructor,
                    values,
                },
                Pattern::Constructor { name, arguments },
            ) => {
                if constructor != name {
                    return false;
                }

                // ??
                if values.len() != arguments.len() {
                    return false;
                }

                values
                    .iter()
                    .zip(arguments)
                    .all(|(value, argument)| self.fits_pattern(value, argument))
            }
            (_, Pattern::All(_)) => true,
            (Value::Function(_), Pattern::Constructor { .. }) => false,
            (Value::Constructor(_), Pattern::Constructor { .. }) => false,
        }
    }

    fn define_pattern_locals(&mut self, value: Value, pattern: Pattern) {
        match (value, pattern) {
            (value, Pattern::All(name)) => self.locals.push((name, value)),
            (
                Value::Basic {
                    constructor: _,
                    values,
                },
                Pattern::Constructor { name: _, arguments },
            ) => {
                for (value, argument) in values.into_iter().zip(arguments) {
                    self.define_pattern_locals(value, argument);
                }
            }
            (Value::Function(_), Pattern::Constructor { .. }) => (),
            (Value::Constructor(_), Pattern::Constructor { .. }) => (),
        }
    }

    fn eval_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Word(word) => match self.resolve_word(word) {
                value @ Value::Basic { .. } => {
                    self.stack.push(value);
                }
                Value::Constructor(arity) => {
                    let values = self.stack.split_off(self.stack.len() - arity);
                    self.stack.push(Value::Basic {
                        constructor: word.clone(),
                        values,
                    })
                }
                Value::Function(branches) => {
                    for Branch { patterns, body } in branches {
                        if self.stack[self.stack.len() - patterns.len()..]
                            .iter()
                            .zip(&patterns)
                            .all(|(value, pattern)| self.fits_pattern(value, pattern))
                        {
                            let locals_len = self.locals.len();
                            let values = self.stack.split_off(self.stack.len() - patterns.len());
                            for (value, pattern) in values.into_iter().zip(patterns) {
                                self.define_pattern_locals(value, pattern)
                            }
                            for expr in body {
                                self.eval_expr(&expr);
                            }
                            self.locals.truncate(locals_len);
                            return;
                        }
                    }
                    panic!("Non exhaustive patterns")
                }
            },
        }
    }

    fn eval_top_levels(&mut self, top_levels: &[TopLevel]) {
        for top_level in top_levels {
            match top_level {
                TopLevel::Data {
                    name: _,
                    constructors,
                } => {
                    for Constructor {
                        name,
                        argument_types,
                    } in constructors
                    {
                        self.ctx
                            .insert(name.clone(), Value::Constructor(argument_types.len()));
                    }
                }
                TopLevel::Def {
                    name,
                    type_expr: _,
                    branches,
                } => {
                    self.ctx
                        .insert(name.clone(), Value::Function(branches.clone()));
                }
            }
        }
    }

    pub fn eval_from_main(&mut self, top_levels: &[TopLevel]) {
        self.eval_top_levels(top_levels);

        let Some(Value::Function(branches)) = self.ctx.get("main").cloned() else {
            panic!();
        };

        let [Branch { patterns: _, body }] = &branches[..] else {
            panic!()
        };

        for expr in body {
            self.eval_expr(expr);
        }

        for value in &self.stack {
            println!("{value:?}")
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Basic {
        constructor: String,
        values: Vec<Value>,
    },
    Function(Vec<Branch>),
    Constructor(usize),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Basic {
                constructor,
                values,
            } => {
                if values.is_empty() {
                    write!(f, "{constructor}")
                } else {
                    write!(f, "{constructor}{values:?}")
                }
            },
            Value::Function(_) => todo!(),
            Value::Constructor(_) => todo!(),
        }
    }
}
