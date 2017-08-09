use std::io;
use std::io::Read;

#[derive(Debug)]
struct Ctx {
    n : i32, // total number of airports
    k : i32, // starting airport 0..n-1
    fs : Vec<(i32, i32)>, // cities connected by flights, undirected
}

fn main() {
    let ctx = parse_input();
    let res = 0;
    println!("{}", res);
}

// input is like
// 4 3                                                       2
// 3 2
// 3 1
// 1 4
fn parse_input() -> Ctx {
    let mut input = String::new();
    let mut idx = 0u32;
    println!("read_to_string");
    let result = io::stdin().read_to_string(&mut input);
    println!("imput = {:?}", input);
    let n = parse_i32(&input, &mut idx);
    unimplemented!()
}

fn parse_i32(input: &str, idx: &mut u32) -> i32 {
    // iterate symbols in str until
    for s in input.split_whitespace() {
        println!("s = {}", s)
    }
    0
//    let mut input = String::new();
//
//    io::stdin()
//        .read_line(&mut input)
//        .expect("correct input");
//    let res = input
//        .trim()
//        .split(' ')
//        .map(|a| a.parse::<i32>())
//        .map(|a| a.expect("parsed integer"))
//        .fold(0i32, |sum, a| sum + a);
//
}