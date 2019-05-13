#[macro_use] extern crate maplit;

use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::collections::HashMap;

#[derive(Copy, Clone, PartialEq, Debug)]
enum Arity {
    Unary,
    Binary,
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Assoc {
    Left,
    Right,
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Brace {
    Round,
    Square
}

#[derive(Copy, Clone, PartialEq, Debug)]
struct Op {
    symbol: char,
    ary: Arity,
    assoc: Assoc,
    prec: u8,
}

#[derive(Clone, PartialEq, Debug)]
enum Token {
    Number(f32),
    RealVar(String),
    BoolVar(String),
    Operator(Op),
    LeftParen(Brace),
    RightParen(Brace),
    Function(String),
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Value {
    Real(f32),
    Bool(bool)
}

#[derive(Clone, PartialEq, Debug)]
enum Error {
    Lex(char),
    Rpn(char),
    Eval(String)
}

impl Token {
    fn binary_left_assoc(c: char, prec: u8) -> Token {
        Token::Operator(
            Op {
                ary: Arity::Binary,
                symbol: c,
                assoc: Assoc::Left,
                prec,
            })
    }
    fn unary_right_assoc(c: char, prec: u8) -> Token {
        Token::Operator(
            Op {
                ary: Arity::Unary,
                symbol: c,
                assoc: Assoc::Right,
                prec,
            })
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::Lex(c) => write!(f, "{}", c),
            Error::Rpn(c) => write!(f, "{}", c),
            Error::Eval(s) => write!(f, "{}", s),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Token::Number(n) => write!(f, "{}", n),
            Token::RealVar(id) => write!(f, "{}", id),
            Token::BoolVar(id) => write!(f, "{}", id),
            Token::Operator(op) => if op.ary == Arity::Unary {
                // unary `-` and `+` we display as `m` and `p`
                let x = match op.symbol {
                    '-' => 'm',
                    '+' => 'p',
                    _ => op.symbol
                };
                write!(f, "{}", x)
            } else {
                write!(f, "{}", op.symbol)
            },
            Token::LeftParen(b) => {
                let c = match b {
                    Brace::Square => '[',
                    Brace::Round => '(',
                };
                write!(f, "{}", c)
            },
            Token::RightParen(b) => {
                let c = match b {
                    Brace::Square => ']',
                    Brace::Round => ')',
                };
                write!(f, "{}", c)
            },
            Token::Function(name) => write!(f, "{}", name),
        }
    }
}

fn main() -> Result<(), Error> {
//    let input = "- 1 * - [2 + 3] + 4 * [- 5 * 6] + 7 * - 8";
//    let input = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3";
//    let input = "ln ( exp ( 1 ) / 2 * ( 3 + 4 ) )";
    let input = "ln(R0) + ln(exp(R1) * 2 > 1 & Bx)";
    let tokens = lex(input)?;
    let rpn = rpn(&tokens[..], true)?;
    let bindings = hashmap!{
        "R0" => Value::Real(1.0),
        "R1" => Value::Real(0.5),
        "Bx" => Value::Bool(true),
    };
    let result = eval(&rpn[..], &bindings, false);
    let rpn = (&rpn).into_iter().map(|t| format!(" {}", t)).collect::<String>();
    println!("--------------------------");
    println!("Input: {}", input);
    println!("Output: {}", rpn);
    println!("Bindings: {:?}", bindings);
    match result {
        Ok(ok) => println!("Eval success: {}", ok),
        Err(err) => println!("Eval error: {}", err),
    };
    Ok(())
}

fn lex(input: &str) -> Result<Vec<Token>, Error> {
    let mut result: Vec<Token> = Vec::with_capacity(input.len());
    let mut it = input.chars().peekable();
    // set to true if some operand has been seen
    let mut is_binary = false;
    // set to true, if function name seen and we expect symbol '(' only
    let mut was_function = false;

    while let Some(c) = it.next() {
        // if the current char is whitespace, just skip
        if !c.is_whitespace() {
            // we have seen function as the previous token, expect open parentheses only
            if was_function {
                let t = match c {
                    '(' => {
                        was_function  = false;
                        Token::LeftParen(Brace::Round)
                    },
                    _ => return Err(Error::Lex(c)),
                };
                result.push(t);
            } else {
                let t = match c {
                    '0'..='9' => Token::Number(get_number(&mut it, c)),
                    '+' | '-' => if is_binary { Token::binary_left_assoc(c, 5) } else { Token::unary_right_assoc(c, 8) },
                    '*' | '/' => Token::binary_left_assoc(c, 7),
                    '^'       => Token::binary_left_assoc(c, 6),
                    '<' | '>' => Token::binary_left_assoc(c, 4),
                    '=' | '#' => Token::binary_left_assoc(c, 4),
                    '~'       => Token::unary_right_assoc(c, 3),
                    '&'       => Token::binary_left_assoc(c, 2),
                    '!'       => Token::binary_left_assoc(c, 2),
                    'R'       => Token::RealVar(get_identifier(&mut it, c)),
                    'B'       => Token::BoolVar(get_identifier(&mut it, c)),
                    '['       => Token::LeftParen(Brace::Square),
                    ']' | ')' => Token::RightParen(if c == ']' { Brace::Square } else { Brace::Round }),
                    'a'..='z'  => match get_function(&mut it, c) {
                        Ok(fun) => {
                            was_function = true;
                            Token::Function(fun)
                        },
                        Err(c) => return Err(Error::Lex(c)),
                    },
                    _ => return Err(Error::Lex(c)),
                };
                // if we have seen an operand, then we expect the binary operator
                is_binary = match c {
                    '0'..='9' => true,
                    'R' => true,
                    'B' => true,
                    ']' => true,
                    ')' => true,
                    _ => false,
                };
                result.push(t);
            }
        }
    }
    Ok(result)
}

fn rpn(input: &[Token], debug: bool) -> Result<Vec<&Token>, Error> {
    let mut stack: Vec<&Token> = Vec::new(); // holds operators and left brackets
    let mut output: Vec<&Token> = Vec::with_capacity(input.len());
    if debug {
        println!("{:^7}|{:^40}|{:^30}", "token", "output", "stack");
        println!("{:^7}+{:^40}+{:^30}", "------", "------------", "------------");
    }
    for token in input {
        match token {
            Token::Number(_) => output.push(token),
            Token::RealVar(_) => output.push(token),
            Token::BoolVar(_) => output.push(token),
            Token::LeftParen(_) => stack.push(token),
            Token::Function(_) => stack.push(token),
            Token::Operator(op) => {
                while let Some(&top) = stack.last() {
                    let cond = match top {
                        // the top of the stack is not a left parenthesis AND
                        Token::LeftParen(_) => break,
                        // the top of the stack is a function
                        Token::Function(_) => true,
                        // the top of the stack has greater precedence than op
                        Token::Operator(top) if top.prec > op.prec => true,
                        // the top of the stack has equal precedence to op and is left associative
                        Token::Operator(top) if top.prec == op.prec && top.assoc == Assoc::Left => true,
                        // otherwise
                        _ => false,
                    };
                    if cond {
                        // pop operators from the operator stack onto the output queue
                        // we know for sure the stack.pop() doesn't fail here
                        output.push(stack.pop().unwrap());
                    } else {
                        // this operation should remain in the stack, put it back
                        break;
                    }
                }
                stack.push(token);
            },
            Token::RightParen(cur) => {
                if stack.is_empty() {
                    // braces are not balanced
                    return Err(Error::Rpn(if *cur == Brace::Square { ']' } else { ')' }));
                }
                while let Some(ref top) = stack.last() {
                    match top {
                        Token::LeftParen(top) => {
                            if *cur != *top {
                                return Err(Error::Rpn(if *cur == Brace::Square { ']' } else { ')' }))
                            }
                            // consume the left paren and break
                            stack.pop();
                            break;
                        },
                        _ => {
                            // we know for sure the top exists and is not a LeftBracket
                            output.push(stack.pop().unwrap());
                        },
                    }
                }
            },
        };
        if debug {
            let str_stack = stack.iter().map(|t| format!(" {}", t)).collect::<String>();
            let str_output = output.iter().map(|t| format!(" {}", t)).collect::<String>();
            println!("{:<7}|{:<40}|{:<30}", token.to_string(), str_output, str_stack);
        }
    }
    // after the loop, if operator stack not empty, pop everything to output queue
    while let Some(t) = stack.last() {
        match t {
            Token::LeftParen(_) => break,
            _ => {},
        }
        output.push(stack.pop().unwrap());
    }
    if debug {
        let str_stack = stack.iter().map(|t| format!(" {}", t)).collect::<String>();
        let str_output = output.iter().map(|t| format!(" {}", t)).collect::<String>();
        println!("{:<7}|{:<40}|{:<30}", "", str_output, str_stack);
    }
    // if stack still is not empty, then braces are not balanced
    if stack.is_empty() { Ok(output) } else { Err(Error::Rpn(')')) }
}

fn eval(tokens: &[&Token], bindings: &HashMap<&str, Value>, is_debug: bool) -> Result<Value, Error> {
    let mut stack: Vec<Value> = vec![];
    if is_debug {
        println!("{:^7}|{:^40}", "token", "stack");
        println!("{:^7}+{:^40}", "------", "------------");
    }
    for token in tokens.iter() {
        match token {
            Token::Number(n) => stack.push(Value::Real(*n)),
            Token::RealVar(var) => match bindings.get(&var[..]) {
                Some(x@Value::Real(_)) => stack.push(*x),
                Some(x@Value::Bool(_)) => return Err(Error::Eval(format!("Type error: real variable {} has value {:?}", var, x))),
                _ => return Err(Error::Eval(format!("Variable not defined: {}", var)))
            },
            Token::BoolVar(var) => match bindings.get(&var[..]) {
                Some(x@Value::Bool(_)) => stack.push(*x),
                Some(x@Value::Real(_)) => return Err(Error::Eval(format!("Type error: boolean variable {} has value {:?}", var, x))),
                _ => return Err(Error::Eval(format!("Variable not defined: {}", var)))
            },
            Token::Operator(op) => {
                match op.ary {
                    Arity::Unary => eval_unary_operator(op, &mut stack)?,
                    Arity::Binary => eval_binary_operator(op, &mut stack)?,
                }
            },
            Token::Function(fun) => eval_function(fun, &mut stack)?,
            _ => return Err(Error::Eval(format!("Unexpected token in RPN: {}", token))),
        }
        if is_debug {
            let str_stack = stack.iter().map(|t| format!(" {}", t)).collect::<String>();
            println!("{:<7}|{:<40}", token.to_string(), str_stack);
        }
    }
    if is_debug {
        // print final state of the stack
        let str_stack = stack.iter().map(|t| format!(" {}", t)).collect::<String>();
        println!("{:<7}|{:<40}", "", str_stack);
    }
    // the stack size should be exactly 1
    if stack.len() == 1 {
        Ok(stack.pop().unwrap())
    } else if stack.is_empty() {
        Err(Error::Eval("The expression is empty".into()))
    } else {
        Err(Error::Eval("The expression is not exhaustive".into()))
    }
}

fn get_number<I: Iterator<Item = char>>(it: &mut Peekable<I>, c: char) -> f32 {
    let mut n = c.to_digit(10).expect("gen_number invariant violation") as f32;
    while let Some(Some(k)) = it.peek().map(|c| c.to_digit(10)) {
        n = n * 10.0 + (k as f32);
        it.next();
    }
    // maybe there is a fractional part of the number
    if it.peek() == Some(&'.') {
        it.next();
    }
    let mut d = 1.0;
    while let Some(Some(k)) = it.peek().map(|c| c.to_digit(10)) {
        d = d * 10.0;
        n = n + (k as f32) / d;
        it.next();
    }
    n
}

fn get_identifier<I: Iterator<Item = char>>(it: &mut Peekable<I>, c: char) -> String {
    let mut id = c.to_string();
    while let Some(s) = it.peek().filter(|c| c.is_alphanumeric()) {
        id += &(s.to_string());
        it.next();
    }
    id
}

fn get_function<I: Iterator<Item = char>>(mut it: &mut Peekable<I>, c: char) -> Result<String, char> {
    let fun = get_identifier(&mut it, c);
    if fun == "exp" || fun == "ln" {
        Ok(fun)
    } else {
        Err(c)
    }
}

fn eval_unary_operator(op: &Op, stack: &mut Vec<Value>) -> Result<(), Error> {
    if let Some(a) = stack.pop() {
        if let Value::Real(a) = a {
            match op.symbol {
                '-' => stack.push(Value::Real(-a)),
                '+' => stack.push(Value::Real(a)),
                _ => return Err(Error::Eval(format!("Unknown real unary operator {}", op.symbol)))
            }
        } else if let Value::Bool(a) = a {
            match op.symbol {
                '~' => stack.push(Value::Bool(!a)),
                _ => return Err(Error::Eval(format!("Unknown boolean unary operator {}", op.symbol)))
            }
        }
    } else {
        return Err(Error::Eval(format!("Not enough values to call unary operator {}", op.symbol)))
    }
    Ok(())
}

fn eval_function(fun: &str, stack: &mut Vec<Value>) -> Result<(), Error> {
    if let Some(a) = stack.pop() {
        if let Value::Real(a) = a {
            match fun {
                "exp" => stack.push(Value::Real(a.exp())),
                "ln" => if a > 0.0 { stack.push(Value::Real(a.ln())) } else {
                    return Err(Error::Eval(format!("Call function '{}' is invalid for argument {}", fun, a)))
                },
                _ => return Err(Error::Eval(format!("Unknown function '{}'", fun)))
            }
        } else {
            return Err(Error::Eval(format!("Type error, expected real value for function '{}', found {:?}", fun, a)))
        }
    } else {
        return Err(Error::Eval(format!("Not enough values to call function {}", fun)))
    }
    Ok(())
}

fn eval_binary_operator(op: &Op, stack: &mut Vec<Value>) -> Result<(), Error> {
    // NOTE: the arguments for the operations are taken from stack in the opposite order
    // e.g. if the original expression is `a / b`, then RPN is `a b /`, therefore we
    // take `b` at first then the second is `a`
    if let Some(b) = stack.pop() {
        if let Some(a) = stack.pop() {
            match (a, b) {
                (Value::Real(a), Value::Real(b)) => {
                    // real functions
                    match op.symbol {
                        '+' => stack.push(Value::Real(a + b)),
                        '-' => stack.push(Value::Real(a - b)),
                        '*' => stack.push(Value::Real(a * b)),
                        '/' => if b != 0.0 { stack.push(Value::Real(a / b)) } else {
                            return Err(Error::Eval(format!("Division by zero")))
                        },
                        '^' => stack.push(Value::Real(a.powf(b))),
                        '=' => stack.push(Value::Bool(a == b)),
                        '#' => stack.push(Value::Bool(a != b)),
                        '>' => stack.push(Value::Bool(a > b)),
                        '<' => stack.push(Value::Bool(a < b)),
                        _ => return Err(Error::Eval(format!("Unknown real binary operator {}", op.symbol)))
                    }
                },
                (Value::Bool(a), Value::Bool(b)) => {
                    // boolean functions
                    match op.symbol {
                        '&' => stack.push(Value::Bool(a && b)),
                        '!' => stack.push(Value::Bool(a || b)),
                        _ => return Err(Error::Eval(format!("Unknown boolean binary operator '{}'", op.symbol)))
                    }
                },
                _ => return Err(Error::Eval(format!("Type error to call binary operator '{}' {:?} {:?}", op.symbol, a, b))),
            }
        } else {
            return Err(Error::Eval(format!("Not enough values to call operator '{}'", op.symbol)))
        }
    } else {
        return Err(Error::Eval(format!("Not enough values to call operator '{}'", op.symbol)))
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::Op;
    use super::Token::*;
    use crate::{lex, rpn, Token, Error, Value, eval};
    use crate::Arity::*;
    use crate::Assoc::*;
    use crate::Brace::*;
    use crate::Value::*;
    use std::collections::HashMap;

    #[test]
    fn unary_detect() {
        let expected = vec![
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '+', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Number(1.0),
            Operator(Op { symbol: '*', ary: Binary, assoc: Left, prec: 7 }),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '+', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            RealVar("R1".to_string()),
        ];
        assert_eq!(lex("- + - 1 * - + - R1").unwrap(), expected);
    }

    #[test]
    fn long_unary_chains() {
        let input = "+ - - 1.234 * + - - 2.345";
        let tokens = lex(input).unwrap();
        let expected = vec![
            Number(1.234),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '+', ary: Unary, assoc: Right, prec: 8 }),
            Number(2.345),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '+', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '*', ary: Binary, assoc: Left, prec: 7 }),
        ];
        let actual: Vec<Token> = rpn(&tokens[..], false)
            .unwrap()
            .into_iter()
            .cloned()
            .collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn brackets_vs_braces_pos() {
        let tokens = lex("ln ( exp ( 1.234 ) / 2 * [ 3 + 4 ]) ").unwrap();
        let expected: Vec<Token> = vec![
            Function("ln".to_string()),
            LeftParen(Round),
            Function("exp".to_string()),
            LeftParen(Round),
            Number(1.234),
            RightParen(Round),
            Operator(Op { symbol: '/', ary: Binary, assoc: Left, prec: 7 }),
            Number(2.0),
            Operator(Op { symbol: '*', ary: Binary, assoc: Left, prec: 7 }),
            LeftParen(Square),
            Number(3.0),
            Operator(Op { symbol: '+', ary: Binary, assoc: Left, prec: 5 }),
            Number(4.0),
            RightParen(Square),
            RightParen(Round)
        ];
        assert_eq!(expected, tokens)
    }

    #[test]
    fn brackets_vs_braces_neg() {
        let tokens1 = lex("ln ( exp ( 1.234 ) / 2 * ( 3 + 4 )) ");
        let tokens2 = lex("ln [ exp ( 1.234 ) / 2 * ( 3 + 4 )] ");
        let tokens3 = lex("ln ( exp 1.234  / 2 * [ 3 + 4 ]) ");
        let tokens4 = lex("ln ( exp ( 1.234 ) / 2 * ( 3 + 4 ]) ");
        let tokens5 = lex("ln ( exp ( 1.234 ) / 2 * [ 3 + 4 )) ");
        assert_eq!(Err(Error::Lex('(')), tokens1);
        assert_eq!(Err(Error::Lex('[')), tokens2);
        assert_eq!(Err(Error::Lex('1')), tokens3);
        assert_eq!(Err(Error::Lex('(')), tokens4);
        // we cannot check the balance in the lexer, lex computed an Ok value
        assert_eq!(std::mem::discriminant(&tokens5), std::mem::discriminant(&Ok(vec![])));
    }

    #[test]
    fn brackets_vs_braces_balance() {
        let tokens = lex("ln ( exp ( 1.234 ) / 2 * [ 3 + 4 )) ").unwrap();
        let tokens = rpn(&tokens[..], false);
        let expected = Err(Error::Rpn(')'));
        assert_eq!(expected, tokens);
    }

    #[test]
    fn eval_logic() {
        let bindings = hashmap!{"BF" => Bool(false), "BT" => Bool(true)};
        let result = test_full_chain("1 = 1 & BF ! ~~~BT", &bindings, false);
        assert_eq!(Ok(Bool(false)), result);

        let result = test_full_chain("1 = 1 & 1 # 2", &bindings, false);
        assert_eq!(Ok(Bool(true)), result);
    }

    #[test]
    fn evalz() {
        let bindings = hashmap!{"R0" => Real(0.5), "R1" => Real(1.0), "Bt" => Bool(true)};
        let res = test_full_chain("exp(R0) / ln(R1)", &bindings, false);
        assert_eq!(Err(Error::Eval("Division by zero".into())), res);

        let res = test_full_chain("exp(R0) / [ln(R1) = 1]", &bindings, true);
        assert_eq!(Err(Error::Eval("Type error to call binary operator '/' Real(1.6487212) Bool(false)".into())), res);

        let res = test_full_chain("exp(R0) > [ln(R1) + 1.6] * Bt ", &bindings, true);
        assert_eq!(Err(Error::Eval("Type error to call binary operator '*' Real(1.6) Bool(true)".into())), res);

        let res = test_full_chain("R0 R0 + R1", &bindings, true);
        assert_eq!(Err(Error::Eval("The expression is not exhaustive".into())), res);

        let res = test_full_chain("+ + +", &bindings, true);
        assert_eq!(Err(Error::Eval("Not enough values to call unary operator +".into())), res);
    }

    fn test_full_chain(input: &str, bindings: &HashMap<&str, Value>, is_debug: bool) -> Result<Value, Error> {
        let tokens = lex(input)?;
        let rpn = rpn(&tokens[..], is_debug)?;
        eval(&rpn[..], &bindings, is_debug)
    }
}