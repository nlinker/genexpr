use std::cmp::Ordering::{Greater, Less};
use binary_tree::BTree::{Empty, Leaf};

#[derive(Debug)]
pub enum BTree<T: Ord> {
    Empty,
    Leaf {
        t: T,
        l: Box<BTree<T>>,
        r: Box<BTree<T>>,
    }
}

impl<T: Ord> BTree<T> {
    pub fn new() -> BTree<T> {
        Empty
    }

    pub fn insert(&mut self, s: T) {
        match self {
            &mut Leaf {ref t, ref mut l, ref mut r} => {
                // OR match s.cmp(t) {..}
                match t.cmp(&s) {
                    Greater => l.insert(s),
                    Less    => r.insert(s),
                    _                 => return
                }
            },
            &mut Empty => {
                *self = Leaf{
                    t: s,
                    l: Box::new(Empty),
                    r: Box::new(Empty),
                }
            },
        }
    }
}

pub fn main() {
    let mut root = BTree::new();
    root.insert("b");
    root.insert("a");
    root.insert("c");
    println!("{:?}", root);
}