use std::collections::HashMap;

const N: i32 = 20;
const M: i32 = 7;

fn main() {
    let mut xs: HashMap<i32, bool> = HashMap::new();
    for j in 1..(N + 1) {
        xs.insert(j, true); // alive
    }
    let mut s = 1;
    for _j in 1..N {
        s = next(&mut xs, s);
    }
    println!("s:{}\te:{}\tcount:{}\t xs:{:?}", s, xs[&s], count(&xs), xs);
    println!("The last one is {}", s);
}

//fn count(xs: &HashMap<i32, bool>) -> i32 {
//    xs.iter().map(|(_k, v)| if *v {1} else {0}).sum()
//}

fn count(xs: &HashMap<i32, bool>) -> i32 {
    xs.values().filter(|v| **v).count() as i32

fn next(xs: &mut HashMap<i32, bool>, start: i32) -> i32 {
    // start in 1..n, and xs.get(start) == true
    assert!(0 < start);
    assert_eq!(xs[&start], true);
    let mut s = start;
    for _i in 1..M {
        s = find(&xs, plus(s, 1)).unwrap();
    }
    xs.insert(s, false);
    println!("s:{}\te:{}\tcount:{}", s, xs[&s], count(&xs));
    find(&xs, s).unwrap()
}

fn plus(s: i32, j: i32) -> i32 {
    (s + j - 1) % N + 1
}

// find first alive starting from start
fn find(xs: &HashMap<i32, bool>, start: i32) -> Option<i32> {
    for j in 0..N {
        let c = plus(start, j);
        if xs[&c] {
            return Some(c);
        }
    }
    None
}
