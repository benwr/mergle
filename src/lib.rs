use std::rc;
use std::cmp::Ordering;

use blake3;

use num_bigint::BigUint;

type Rc<T> = rc::Rc<T>;

struct MergleInternalNode<T> {
    height: usize,
    hash: blake3::Hash,
    children: Vec<Rc<MergleNode<T>>>,
}

struct MergleLeaf<T> {
    content: Vec<T>,
    hash: blake3::Hash,
}

enum MergleNode<T> {
    Internal(MergleInternalNode<T>),
    Leaf(MergleLeaf<T>),
}

pub struct Mergle<T> {
    root: MergleNode<T>,
}

const CHUNK_MASK: usize = !0b111;

// hm, is there a principled way to choose this number?
const WINDOW_SIZE: usize = 131;

impl<T> Mergle<T> {
    pub fn singleton(t: T) -> Mergle<T> {
        panic!()
    }

    pub fn merge(&self, other: &Self) -> Self {
        panic!()
    }
}

impl<T> PartialEq for Mergle<T> {
    fn eq(&self, other: &Self) -> bool {
        panic!()
    }
}

impl<T> Eq for Mergle<T> {}

impl<T: PartialOrd> PartialOrd for Mergle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        panic!()
    }
}

impl<T: Ord> Ord for Mergle<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        panic!()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
