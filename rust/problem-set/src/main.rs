//#[macro_use]
//extern crate lazy_static;

use std::io;
use std::io::Read;
use std::iter::FromIterator;

#[derive(Debug)]
struct Ctx {
    // total number of airports
    n: i32,
    // starting airport 0..n-1
    k: i32,
    // cities connected by flights, undirected
    fs: Vec<(i32, i32)>,
}

fn main() {
    let ctx = parse_input();
    println!("{:?}", ctx);
}

//lazy_static! {
//    static ref CTX0: Ctx = Ctx {
//        n: 4,
//        k: 3,
//        fs: vec![(3, 2), (3, 1), (1, 4)],
//    };
//}

// input is like
//4 3
//3 2
//3 1
//1 4
fn parse_input() -> Ctx {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .expect("correct input");
    let parts = Vec::from_iter(input.split_whitespace());
    // now let's read the Ctx
    let mut idx: i32 = 0;
    let n = parse_i32(&parts, &mut idx);
    let k = parse_i32(&parts, &mut idx);
    let mut fs: Vec<(i32, i32)> = Vec::new();
    for _i in 1..n {
        let a = parse_i32(&parts, &mut idx);
        let b = parse_i32(&parts, &mut idx);
        fs.push((a, b));
    }
    Ctx { n, k, fs }
}

fn parse_i32(parts: &Vec<&str>, idx: &mut i32) -> i32 {
    let r = parts[*idx as usize]
        .parse::<i32>()
        .expect("parsed integer");
    *idx += 1;
    r
}