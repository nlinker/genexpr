#![allow(unused)]
#![feature(type_ascription)]

use crate::cactusref_graph::{circular_graph, fully_connected_graph, Node};
use cactusref::Rc;
use std::cell::{RefCell, Ref};
use log::{trace, Level};

fn main() {
    simple_logger::init_with_level(Level::Trace).unwrap();
    trace!("");
    let g1 = circular_graph(5);
    for i in 0..g1.len() {
        let links = &g1[i].borrow().links.iter().map(|rc: &Rc<RefCell<Node<usize>>>| {
            let node_ref: &Node<usize> = &rc.borrow();
            let node_link: usize = node_ref as *const _ as usize;
            //
            format!("Rc({:x}, {}s, {}w, {:?}, {:?})",
                    node_link,
                    Rc::strong_count(rc),
                    Rc::weak_count(rc),
                    rc.dbg_fwd_links(),
                    rc.dbg_back_links()
            )
        }).collect::<Vec<_>>();

        let rc: &Rc<RefCell<Node<usize>>> = &g1[i];
        // let rc_box: &RcBox<RefCell<Node<usize>>> = rc.inner();
        // let fwd_links: RefCell<Links<RefCell<Node<usize>>>> = unsafe { rc.ptr.as_ref().links };
        let node_ref: &Node<usize> = &*g1[i].borrow();
        let node_link: usize = node_ref as *const _ as usize;
        println!("&Rc({:x}), Rc({:x}, {}s, {}w, {:?}, {:?}), links: {:?}",
                 &g1[i] as *const _ as usize,
                 node_link,
                 Rc::strong_count(rc),
                 Rc::weak_count(rc),
                 rc.dbg_fwd_links(),
                 rc.dbg_back_links(),
                 links,
        );
    }
    fully_connected_graph(5);
}

mod cactusref_test {
    use cactusref::{Adoptable, Rc};
    use std::cell::{RefCell};
    use std::iter;
    // use std::borrow::BorrowMut; // Don't enable otherwise it fails to compile

    struct Node<T> {
        prev: Option<Rc<RefCell<Self>>>,
        next: Option<Rc<RefCell<Self>>>,
        data: T,
    }

    struct List<T> {
        pub head: Option<Rc<RefCell<Node<T>>>>,
    }

    impl<T> List<T> {
        fn pop(&mut self) -> Option<Rc<RefCell<Node<T>>>> {
            let head: Rc<RefCell<Node<T>>> = self.head.take().unwrap();
            let tail = head.borrow_mut().prev.take();
            let next = head.borrow_mut().next.take();

            if let Some(ref tail) = tail {
                Rc::unadopt(&head, &tail);
                Rc::unadopt(&tail, &head);
                tail.borrow_mut().next = next.as_ref().map(Rc::clone);
                if let Some(ref next) = next {
                    Rc::adopt(tail, next);
                }
            }
            if let Some(ref next) = next {
                Rc::unadopt(&head, &next);
                Rc::unadopt(&next, &head);
                next.borrow_mut().prev = tail.as_ref().map(Rc::clone);
                if let Some(ref tail) = tail {
                    Rc::adopt(next, tail);
                }
            }
            self.head = next;
            Some(head)
        }
    }

    impl<T> From<Vec<T>> for List<T> {
        fn from(list: Vec<T>) -> Self {
            let nodes = list
                .into_iter()
                .map(|data| {
                    Rc::new(RefCell::new(Node {
                        prev: None,
                        next: None,
                        data,
                    }))
                })
                .collect::<Vec<_>>();
            for i in 0..nodes.len() - 1 {
                let curr = &nodes[i];
                let next = &nodes[i + 1];
                curr.borrow_mut().next = Some(Rc::clone(next));
                next.borrow_mut().prev = Some(Rc::clone(curr));
                Rc::adopt(curr, next);
                Rc::adopt(next, curr);
            }
            let tail = &nodes[nodes.len() - 1];
            let head = &nodes[0];
            tail.borrow_mut().next = Some(Rc::clone(head));
            head.borrow_mut().prev = Some(Rc::clone(tail));
            Rc::adopt(tail, head);
            Rc::adopt(head, tail);

            let head = Rc::clone(head);
            Self { head: Some(head) }
        }
    }

    pub fn run_cactusref_test() {
        let list = iter::repeat(())
            .map(|_| "a".repeat(1024 * 1024))
            .take(10)
            .collect::<Vec<_>>();
        let mut list = List::from(list);
        let head = list.pop().unwrap();
        assert_eq!(Rc::strong_count(&head), 1);
        assert_eq!(list.head.as_ref().map(Rc::strong_count), Some(3));
        let weak = Rc::downgrade(&head);
        drop(head);
        assert!(weak.upgrade().is_none());
        drop(list);
    }
}

// ------------------------------------------------------------------------------- //

mod cactusref_graph {
    use cactusref::{Adoptable, Rc};
    use std::cell::RefCell;

    #[derive(Debug)]
    pub struct Node<T> {
        pub links: Vec<Rc<RefCell<Self>>>,
        pub data: T,
    }

    pub fn circular_graph(count: usize) -> Vec<Rc<RefCell<Node<usize>>>> {
        let mut nodes = vec![];
        for i in 0..count {
            nodes.push(Rc::new(RefCell::new(Node {
                data: i,
                links: vec![],
            })));
        }
        for i in 0..count {
            let link = Rc::clone(&nodes[(i + 1) % count]);
            Rc::adopt(&nodes[i], &link);
            nodes[i].borrow_mut().links.push(link);
        }
        nodes
    }

    pub fn fully_connected_graph(count: usize) -> Vec<Rc<RefCell<Node<usize>>>> {
        let mut nodes = vec![];
        for i in 0..count {
            nodes.push(Rc::new(RefCell::new(Node {
                data: i,
                links: vec![],
            })));
        }
        for left in &nodes {
            for right in &nodes {
                let link = Rc::clone(right);
                Rc::adopt(left, &link);
                left.borrow_mut().links.push(link);
            }
        }
        nodes
    }
}
