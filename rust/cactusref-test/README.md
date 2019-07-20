# The task

Evaluate the new `Rc` smart pointer for Rust and implement a linked list in it.

### Cactusref Rc

Taken from [`ferrocarril`](https://github.com/lopopolo/ferrocarril), it is an alternative approach
to do memory management in Rust, it is similar to original `Rc` wrapper, however proposes easier
way to build cyclic data structures. Read tutorial about it.
[in the Ryan Lopopolo's blog](https://hyperbo.la/w/cactus-harvesting/).

Example of the crosslinks in the case of circular graph. 
The graph is of type `Vec<Rc<RefCell<Node<usize>>>>`, and the Node 
contains the links, denoted by `links:`:
```
0: &Rc(55911bf700f0), Rc(55911bf6fe18, 2s, 0w, {"55911bf6fe40": 1}, {"55911bf70050": 1}), 
links: ["Rc(55911bf6feb8, 2s, 0w, {\"55911bf6fee0\": 1}, {\"55911bf6fda0\": 1})"]

1: &Rc(55911bf700f8), Rc(55911bf6feb8, 2s, 0w, {"55911bf6fee0": 1}, {"55911bf6fda0": 1}),
links: ["Rc(55911bf6ff58, 2s, 0w, {\"55911bf6ffb0\": 1}, {\"55911bf6fe40\": 1})"]

2: &Rc(55911bf70100), Rc(55911bf6ff58, 2s, 0w, {"55911bf6ffb0": 1}, {"55911bf6fe40": 1}),
links: ["Rc(55911bf70028, 2s, 0w, {\"55911bf70050\": 1}, {\"55911bf6fee0\": 1})"]

3: &Rc(55911bf70108), Rc(55911bf70028, 2s, 0w, {"55911bf70050": 1}, {"55911bf6fee0": 1}),
links: ["Rc(55911bf700c8, 2s, 0w, {\"55911bf6fda0\": 1}, {\"55911bf6ffb0\": 1})"]

4: &Rc(55911bf70110), Rc(55911bf700c8, 2s, 0w, {"55911bf6fda0": 1}, {"55911bf6ffb0": 1}),
links: ["Rc(55911bf6fe18, 2s, 0w, {\"55911bf6fe40\": 1}, {\"55911bf70050\": 1})"]
```

Each element in the links is Rc:
```rust
    #[derive(Debug)]
    pub struct Node<T> {
        pub links: Vec<Rc<RefCell<Self>>>,
        pub data: T,
    }
```


Notes: Rc has the form `Rc(pointer, strong_count, weak_count, fwd_links, back_links)`.
The `pointer` here is the node, that Rc point to. 
`strong_count` and `weak_count` are obviously strong references count and weak references 
count to the data pointed.

For `fwd_links` we do the followind
```rust
fn test() {
  let fwd_links: RefCell<Links<RefCell<Node<usize>>>> = 
      unsafe { rc.ptr.as_ref().links };
  let fwd_links_unref: Links<RefCell<Node<usize>>> = 
      unsafe { rc.ptr.as_ref().links.as_ptr() };
  let fwd_links_unref_hm: HashMap<Link<RefCell<Node<usize>>>, usize> = 
      unsafe { rc.ptr.as_ref().links.as_ptr().registry };
}
```
So this hash map is the _pointers-to-unsigned_integers_ is being printed out.

```rust
pub struct Links<T: ?Sized> {
    pub registry: HashMap<Link<T>, usize>,
}

pub struct Link<T: ?Sized>(pub NonNull<RcBox<T>>);
```