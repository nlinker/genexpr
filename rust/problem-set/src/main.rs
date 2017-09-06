//#[macro_use]
//extern crate lazy_static;

use std::io;
use std::io::Read;
use std::iter::FromIterator;

#[derive(Debug)]
struct Ctx {
    // total number of airports
    n: i32,
    // starting airport 1..n
    k: i32,
    // cities connected by flights, undirected
    edges: Vec<(i32, i32)>,
}

#[derive(Debug, Clone)]
enum Player {
    White,
    Black,
}

#[derive(Debug, Clone)]
struct Node {
    // current node of the graph
    node: i32,
    player: Player,
    score: i32,
    c_idx: usize,
    c_len: usize,
    parent: Option<usize>
}

fn main() {
    // binary_tree::main();
    let ctx = parse_input();
    match calc(ctx) {
        Some(x) => println!("First player wins flying to airport {}", x),
        None => println!("First player loses"),
    };
}

#[allow(unused_variables)]
fn calc(ctx: Ctx) -> Option<i32> {
    // println!("ctx = {:?}", ctx);
    // build the game tree
    let m = ctx.edges.len();
    let mut tree: Vec<Node> = Vec::with_capacity(2*m);
    let root = Node {
        node: ctx.k,
        player: Player::White,
        score: 0,
        c_idx: 0,
        c_len: 0,
        parent: None,
    };
    tree.push(root);
    // on is the array showing if corresponding edges in the subgraph
    let mut on: Vec<bool> = Vec::with_capacity(m);
    for _e in &ctx.edges {
        on.push(true);
    }
    build_tree(&mut tree, &mut on, &ctx.edges, 0);
    let score = calc_score(&mut tree, 0);
    let start_node = if score > 0 {
        // first player wins, find the airport with the minimal number
        let idx = tree[0].c_idx;
        let len = tree[0].c_len;
        let mut start_node = ctx.n;
        for i in 0..len {
            let node = tree[i + idx].clone();
            if node.score > 0 {
                start_node = min(start_node, node.node);
            }
        }
        Some(start_node)
    } else {
        None
    };
    start_node
}

fn calc_score(
    tree: &mut Vec<Node>,
    ni: usize,
) -> i32 {
    let node = tree[ni].clone();
    if node.c_len == 0 {
        // if terminal node is black, it loses, so score is 1
        // otherwise it wins, therefore the score is -1
        let score = match node.player {
            Player::Black => 1,
            Player::White => -1,
        };
        tree[ni].score = score;
        score
    } else {
        let mut s_min = 99990;
        let mut s_max = -99999;
        for i in 0..node.c_len {
            let score = calc_score(tree, i + node.c_idx);
            s_min = min(s_min, score);
            s_max = max(s_max, score);
        }
        let score = match node.player {
            Player::Black => s_min,
            Player::White => s_max,
        };
        tree[ni].score = score;
        score
    }
}

fn build_tree(
    tree: &mut Vec<Node>,
    on: &mut Vec<bool>,
    edges: &Vec<(i32,i32)>,
    ni: usize,
) {
    let mut adj: Vec<i32> = Vec::with_capacity(10);
    // current node
    let node = tree[ni].clone();
    let from = node.node;
    let player = node.player;
    // index from where the children start
    let idx = tree.len();
    for j in 0..edges.len() {
        let e = edges[j];
        if on[j] && (from == e.0 || from == e.1) {
            on[j] = false;
            adj.push(j as i32);
            let to = if from == e.0 {e.1} else {e.0};
            tree.push(Node {
                node: to,
                player: invert(&player),
                score: 0,
                c_idx: 0,
                c_len: 0,
                parent: Some(ni),
            });
        }
    }
    let len = adj.len();
    tree[ni].c_idx = idx;
    tree[ni].c_len = len;
    for i in 0..len {
        let c_idx = idx + i;
        build_tree(tree, on, edges, c_idx);
    }
    // rollback the edges removal
    for i in 0..len {
        on[adj[i] as usize] = true;
    }
}

fn invert(player: &Player) -> Player {
    match player {
        &Player::Black => Player::White,
        &Player::White => Player::Black,
    }
}

fn min(x: i32, y: i32) -> i32 {
    if x < y {x} else {y}
}

fn max(x: i32, y: i32) -> i32 {
    if x > y {x} else {y}
}

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
    let mut edges: Vec<(i32, i32)> = Vec::new();
    for _i in 1..n {
        let a = parse_i32(&parts, &mut idx);
        let b = parse_i32(&parts, &mut idx);
        edges.push((a, b));
    }
    Ctx { n: n, k: k, edges: edges }
}

fn parse_i32(parts: &Vec<&str>, idx: &mut i32) -> i32 {
    let r = parts[*idx as usize]
        .parse::<i32>()
        .expect("parsed integer");
    *idx += 1;
    r
}

//lazy_static! {
//    static ref CTX0: Ctx = Ctx {
//        n: 4,
//        k: 3,
//        edges: vec![(3, 2), (3, 1), (1, 4)],
//    };
//}
//lazy_static! {
//    static ref CTX0: Ctx = Ctx {
//        n: 5,
//        k: 1,
//        edges: vec![(1, 2), (2, 3), (3, 4), (4, 5), (5, 1), (2, 5), (2, 4)],
//    };
//}
