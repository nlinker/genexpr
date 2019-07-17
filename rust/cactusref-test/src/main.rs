#![allow(unused)]
#![feature(type_ascription)]

use cactusref::{Adoptable, Rc};
use std::cell::RefCell;
use std::iter;
use std::borrow::BorrowMut;

struct Node<T> {
    prev: Option<Rc<RefCell<Self>>>,
    next: Option<Rc<RefCell<Self>>>,
    data: T,
}

struct List<T> {
    head: Option<Rc<RefCell<Node<T>>>>,
}

impl<T> List<T> {
    fn pop(&mut self) -> Option<Rc<RefCell<Node<T>>>> {
        let mut head: Rc<RefCell<Node<T>>> = self.head.take().unwrap();
        let x: () = head.borrow_mut().data;

//        let x: &mut T = &mut head.get_mut().data; // uses RefCell::get_mut, cannot borrow as mutable
//        let tail = head.borrow_mut().prev.take();
//        let next = head.borrow_mut().next.take();
//
//        if let Some(ref tail) = tail {
//            Rc::unadopt(&head, &tail);
//            Rc::unadopt(&tail, &head);
//            tail.borrow_mut().next = next.as_ref().map(Rc::clone);
//            if let Some(ref next) = next {
//                Rc::adopt(tail, next);
//            }
//        }
//        if let Some(ref next) = next {
//            Rc::unadopt(&head, &next);
//            Rc::unadopt(&next, &head);
//            next.borrow_mut().prev = tail.as_ref().map(Rc::clone);
//            if let Some(ref tail) = tail {
//                Rc::adopt(next, tail);
//            }
//        }
//        self.head = next;
//        Some(head)
        None
    }
}

impl<T> From<Vec<T>> for List<T> {
    fn from(list: Vec<T>) -> Self {
//        let nodes = list
//            .into_iter()
//            .map(|data| {
//                Rc::new(RefCell::new(Node {
//                    prev: None,
//                    next: None,
//                    data,
//                }))
//            })
//            .collect::<Vec<_>>();
//        for i in 0..nodes.len() - 1 {
//            let curr = &nodes[i];
//            let next = &nodes[i + 1];
//            curr.borrow_mut().next = Some(Rc::clone(next));
//            next.borrow_mut().prev = Some(Rc::clone(curr));
//            Rc::adopt(curr, next);
//            Rc::adopt(next, curr);
//        }
//        let tail = &nodes[nodes.len() - 1];
//        let head = &nodes[0];
//        tail.borrow_mut().next = Some(Rc::clone(head));
//        head.borrow_mut().prev = Some(Rc::clone(tail));
//        Rc::adopt(tail, head);
//        Rc::adopt(head, tail);
//        let head = Rc::clone(head);
//        Self { head: Some(head) }
        Self { head: None }
    }
}

// ------------------------------------------------------------------------------- //
pub fn main() {
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
