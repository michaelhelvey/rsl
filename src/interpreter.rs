#![allow(dead_code)]
use thiserror::Error;

use crate::parser::{Expression, ParseError, Parser, Sexpr, Value};

pub struct Interpreter {}

// Inner interpreter errors
#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("error: {0}")]
    TypeError(String),

    // "Userspace" exception
    #[error("Exception: {0}")]
    Exception(String),
}

mod builtins {
    use super::InterpreterError;
    use crate::parser::{Expression, Value};

    pub fn add_f64(args: Vec<Expression>) -> Result<f64, InterpreterError> {
        let mut result: f64 = 0.0;

        for expr in args {
            match expr {
                Expression::Atom(Value::Number(num)) => {
                    result += num;
                }
                _ => {
                    return Err(InterpreterError::Exception(format!(
                        "invalid argument of type {expr:?} supplied to builtin add_f64"
                    )))
                }
            }
        }

        Ok(result)
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&mut self, expr: Expression) -> Result<Expression, InterpreterError> {
        match expr {
            Expression::Atom(value) => Ok(Expression::Atom(value)),
            Expression::Sexpr(sexpr) => self.eval_sexp(sexpr),
        }
    }

    fn eval_sexp(&mut self, sexp: Box<Sexpr>) -> Result<Expression, InterpreterError> {
        let head = self.eval(sexp.head)?;

        // at the top level, we need an operator in order to understand how to evalulate the rest of
        // the tree pointed to by `tail`:
        match head {
            Expression::Atom(Value::Identifier(operator)) => self.eval_op(&operator, sexp.tail),
            _ => Err(InterpreterError::TypeError(format!(
                "expression of type {head:?} is not callable"
            ))),
        }
    }

    // (fn-name arg1 arg2 arg3)
    fn eval_op(
        &mut self,
        operator: &str,
        tail: Expression,
    ) -> Result<Expression, InterpreterError> {
        match operator {
            "+" => {
                // walk tail get a list of arguments:
                let args = self.walk_arg_list(tail)?;
                // awkward, eventually we probably want something like "eval fn" that knows to 1)
                // get a list of arguments 2) how to apply them to a given function in our "function
                // map" which should be dynamic and not hard-coded into match like this, but we'll
                // do just hard-coded builtins for now
                let result = builtins::add_f64(args)?;

                Ok(Expression::Atom(Value::Number(result)))
            }
            "println" => {
                let args = self.walk_arg_list(tail)?;
                for arg in args {
                    println!("{:?}", arg);
                }

                Ok(Expression::Atom(Value::Nil))
            }
            _ => Err(InterpreterError::TypeError(format!(
                "unbound operator {operator:?}"
            ))),
        }
    }

    fn walk_arg_list(&mut self, tail: Expression) -> Result<Vec<Expression>, InterpreterError> {
        let mut args: Vec<Expression> = vec![];
        self.rec_walk_arg_list(tail, &mut args)?;
        Ok(args)
    }

    fn rec_walk_arg_list(
        &mut self,
        tail: Expression,
        args: &mut Vec<Expression>,
    ) -> Result<(), InterpreterError> {
        // if tail is an Atom(nil), do nothing
        // if Atom(value), push value
        // if Sexpr(_), eval & push head, then append rec_walk_arg_list(tail)

        match tail {
            Expression::Atom(Value::Nil) => {}
            Expression::Atom(value) => args.push(Expression::Atom(value)),
            Expression::Sexpr(sexp) => {
                let head = self.eval(sexp.head)?;
                args.push(head);
                self.rec_walk_arg_list(sexp.tail, args)?;
            }
        };

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Value;
    use color_eyre::Result;

    use super::*;

    fn eval_code(input: &str) -> Result<Expression> {
        let mut parser = Parser::new(input);
        let ast = parser.parse()?;
        let mut interpreter = Interpreter::new();

        Ok(interpreter.eval(ast)?)
    }

    #[test]
    fn test_eval_atom() -> Result<()> {
        let result = eval_code("1")?;
        assert_eq!(result, Expression::Atom(Value::Number(1.0)));

        Ok(())
    }

    #[test]
    fn test_eval_plus() -> Result<()> {
        let result = eval_code("(+)")?;
        assert_eq!(result, Expression::Atom(Value::Number(0.0)));

        let result = eval_code("(+ 1)")?;
        assert_eq!(result, Expression::Atom(Value::Number(1.0)));

        let result = eval_code("(+ 1 (+ 1 2))")?;
        assert_eq!(result, Expression::Atom(Value::Number(4.0)));

        Ok(())
    }
}
