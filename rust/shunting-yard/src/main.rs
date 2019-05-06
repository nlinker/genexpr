
use std::fmt::{Display, Formatter, Error};
use std::iter::Peekable;

#[derive(Copy, Clone, PartialEq, Debug)]
enum Ary {
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
    ary: Ary,
    assoc: Assoc,
    prec: u8,
}

#[derive(Clone, PartialEq, Debug)]
enum Token {
    Number(i32),
    RealVar(String),
    BoolVar(String),
    Operator(Op),
    LeftParen(Brace),
    RightParen(Brace),
    Function(String),
}

impl Token {
    fn binary_left_assoc(c: char, prec: u8) -> Token {
        Token::Operator(
            Op {
                ary: Ary::Binary,
                symbol: c,
                assoc: Assoc::Left,
                prec,
            })
    }
    fn unary_right_assoc(c: char, prec: u8) -> Token {
        Token::Operator(
            Op {
                ary: Ary::Unary,
                symbol: c,
                assoc: Assoc::Right,
                prec,
            })
    }

    fn is_operator(&self) -> bool {
        match self {
            Token::Operator(_) => true,
            _ => false
        }
    }

    fn is_left_paren(&self) -> bool {
        match self {
            Token::LeftParen(_) => true,
            _ => false
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Token::Number(n) => write!(f, "{}", n),
            Token::RealVar(id) => write!(f, "{}", id),
            Token::BoolVar(id) => write!(f, "{}", id),
            Token::Operator(op) => if op.ary == Ary::Unary {
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

fn lex(input: &str) -> Result<Vec<Token>, char> {
    let mut result: Vec<Token> = Vec::with_capacity(input.len());
    let mut it = input.chars().peekable();
    // set to true if some operand have seen
    let mut is_binary = false;

    while let Some(c) = it.next() {
        // if the current char is whitespace, just skip
        if !c.is_whitespace() {
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
                '[' | '(' => Token::LeftParen(if c == '[' { Brace::Square } else { Brace::Round }),
                ']' | ')' => Token::RightParen(if c == ']' { Brace::Square } else { Brace::Round }),
                _ => return Err(c),
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
    Ok(result)
}

fn get_number<I: Iterator<Item = char>>(it: &mut Peekable<I>, c: char) -> i32 {
    let mut n = c.to_digit(10).expect("gen_number invariant violation") as i32;
    while let Some(Some(k)) = it.peek().map(|c| c.to_digit(10)) {
        n = n * 10 + (k as i32);
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

fn rpn(input: &[Token], debug: bool) -> Result<Vec<&Token>, char> {
    let mut stack: Vec<&Token> = Vec::new(); // holds operators and left brackets
    let mut output: Vec<&Token> = Vec::with_capacity(input.len());
    if debug {
        println!("{:10}  {:30}{:30}", "token", "output", "stack");
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
                while let Some(ref top) = stack.last() {
                    match top {
                        Token::LeftParen(top) => {
                            if *cur != *top {
                                return Err(if *cur == Brace::Square { ']' } else { ')' })
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
            println!("{:10}  {:30}{:30}", token, str_output, str_stack);
        }
    }
    // after the loop, if operator stack not empty, pop everything to output queue
    while let Some(t) = stack.pop() {
        if t.is_left_paren() {

        }
        output.push(t);
    }
    Ok(output)
}

fn main() {
//    let input = "- 1 * - [2 + 3] + 4 * [- 5 * 6] + 7 * - 8";
//    let input = "3 + 4 * 2 / ( 1 - 5 ) ^ 6 ^ 7";
    let input = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3";
    let tokens = lex(input).unwrap();
    let rpn = rpn(&tokens[..], true).unwrap();
    let output = (&rpn).into_iter().map(|t| format!(" {}", t)).collect::<String>();
    println!("--------------------------");
    println!("input: {}", input);
    println!("output: {}", output);
}

#[cfg(test)]
mod tests {
    use super::Op;
    use super::Token::*;
    use crate::{lex, rpn, Token};
    use crate::Ary::*;
    use crate::Assoc::*;

    #[test]
    fn unary_detect() {
        let expected = vec![
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '+', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Number(1),
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
        let input = "+ - - 1 * + - - 2";
        let tokens = lex(input).unwrap();
        let expected = vec![
            Number(1),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '+', ary: Unary, assoc: Right, prec: 8 }),
            Number(2),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '-', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '+', ary: Unary, assoc: Right, prec: 8 }),
            Operator(Op { symbol: '*', ary: Binary, assoc: Left, prec: 7 }),
        ];
        let actual: Vec<Token> = rpn(&tokens[..], false)
            .iter()
            .map(|t| *t)
            .cloned()
            .collect();
        assert_eq!(actual, expected);
    }
}