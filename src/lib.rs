use std::rc;
use std::cmp::Ordering;
use std::collections::hash_map::*;

use std::cell::RefCell;
use std::rc::Rc;

use bromberg_sl2::*;

use num_bigint::BigUint;


enum PrefixResult<T> {
    LessThan,
    PrefixOf(Mergle<T>),
    Equal,
    PrefixedBy(Mergle<T>),
    GreaterThan,
}

pub type MemoizationTable<T> = HashMap<(HashMatrix, HashMatrix), PrefixResult<T>>;

struct MergleInternalNode<T> {
    hash: HashMatrix,
    left: Rc<MergleNode<T>>,
    right: Rc<MergleNode<T>>
}

struct MergleLeaf<T> {
    content: T,
    hash: bromberg_sl2::HashMatrix,
}

enum MergleNode<T> {
    Internal(MergleInternalNode<T>),
    Leaf(MergleLeaf<T>),
}

impl<T> MergleNode<T> {
    fn bromberg_hash(&self) -> HashMatrix {
        match self {
            MergleNode::Internal(node) => node.hash,
            MergleNode::Leaf(node) => node.hash
        }
    }
}

pub struct Mergle<T> {
    root: Rc<MergleNode<T>>,
    table: Rc<RefCell<MemoizationTable<T>>>,
}

impl<T: AsRef<[u8]>> Mergle<T> {
    pub fn singleton(t: T, table: Rc<RefCell<MemoizationTable<T>>>) -> Mergle<T> {
        let h = hash(t.as_ref());
        let node = MergleLeaf {
            content: t,
            hash: h
        };

        Mergle {
            root: Rc::new(MergleNode::Leaf(node)),
            table: table,
        }
    }

    pub fn merge(&self, other: &Self) -> Self {
        let hash = self.root.bromberg_hash() * other.root.bromberg_hash();
        let node = MergleInternalNode {
            hash: hash,
            left: Rc::clone(&self.root),
            right: Rc::clone(&other.root)
        };
        Mergle {
            root: Rc::new(MergleNode::Internal(node)),
            table: Rc::clone(&self.table)
        }
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
