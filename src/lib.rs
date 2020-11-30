use std::cmp::Ordering;
use std::collections::hash_map::HashMap;

use std::cell::RefCell;
use std::rc::Rc;

use bromberg_sl2::*;

enum PrefixDiff<T> {
    LessThan,
    PrefixOf(Rc<Node<T>>),
    Equal,
    PrefixedBy(Rc<Node<T>>),
    GreaterThan,
}

impl<T> PrefixDiff<T> {
    fn inverse(self) -> PrefixDiff<T> {
        match self {
            PrefixDiff::LessThan => PrefixDiff::GreaterThan,
            PrefixDiff::PrefixOf(suffix) => PrefixDiff::PrefixedBy(suffix),
            PrefixDiff::Equal => PrefixDiff::Equal,
            PrefixDiff::PrefixedBy(suffix) => PrefixDiff::PrefixOf(suffix),
            PrefixDiff::GreaterThan => PrefixDiff::LessThan,
        }
    }
}

impl<T> Clone for PrefixDiff<T> {
    fn clone(&self) -> Self {
        match self {
            PrefixDiff::PrefixOf(suffix) => PrefixDiff::PrefixedBy(suffix.clone()),
            PrefixDiff::PrefixedBy(suffix) => PrefixDiff::PrefixOf(suffix.clone()),
            PrefixDiff::LessThan => PrefixDiff::LessThan,
            PrefixDiff::Equal => PrefixDiff::Equal,
            PrefixDiff::GreaterThan => PrefixDiff::GreaterThan,
        }
    }
}

pub struct MemTableRef<T>(
    Rc<RefCell<HashMap<(HashMatrix, HashMatrix), PrefixDiff<T>>>>);

impl<T> Clone for MemTableRef<T> {
    fn clone(&self) -> Self {
        MemTableRef(Rc::clone(&self.0))
    }
}

impl<T> MemTableRef<T> {
    pub fn new() -> Self {
        MemTableRef(Rc::new(RefCell::new(HashMap::new())))
    }

    fn insert(&self, a: HashMatrix, b: HashMatrix, r: PrefixDiff<T>) {
        let mut table = self.0.borrow_mut();
        if a > b {
            table.insert((b, a), r.inverse());
        } else {
            table.insert((a, b), r.clone());
        }
    }

    fn lookup(&self, a: HashMatrix, b: HashMatrix) -> Option<PrefixDiff<T>> {
        if a == b {
            Some(PrefixDiff::Equal)
        } else {
            let table = self.0.borrow();
            if a > b {
                table.get(&(b, a)).map(|r| r.clone().inverse())
            } else {
                table.get(&(a, b)).map(|r| r.clone())
            }
        }
    }
}

struct InternalNode<T> {
    hash: HashMatrix,
    left: Rc<Node<T>>,
    right: Rc<Node<T>>,
}

struct LeafNode<T> {
    content: T,
    hash: HashMatrix,
}

enum Node<T> {
    Internal(InternalNode<T>),
    Leaf(LeafNode<T>),
}

impl<T> BrombergHashable for Node<T> {
    fn bromberg_hash(&self) -> HashMatrix {
        match self {
            Node::Internal(node) => node.hash,
            Node::Leaf(node) => node.hash,
        }
    }
}

impl<T: Ord + BrombergHashable> Node<T> {
    fn iter(&self) -> Iter<T> {
        panic!()
    }

    fn prefix_cmp(&self, other: &Self, table: &MemTableRef<T>) -> PrefixDiff<T> {
        let my_hash = self.bromberg_hash();
        let their_hash = other.bromberg_hash();
        if let Some(result) = table.lookup(my_hash, their_hash) {
            return result;
        }
        // TODO perhaps this should recurse in a more principled way. I'm not sure
        // what exactly that way would be, but it might make use of e.g. nodes tracking
        // ther left-height, or maybe their order statistics (though order statistics
        // will grow really fast and make the asymptotics bad, probably).
        let result = match (self, other) {
            (Node::Leaf(leaf), Node::Leaf(other_leaf)) =>
                Mergle::leaf_cmp(&leaf, &other_leaf),
            (Node::Leaf(leaf), _) =>
                other.prefix_cmp(self, table).inverse(),
            (Node::Internal(node), _) => {
                match node.left.prefix_cmp(other, table) {
                    PrefixDiff::LessThan => PrefixDiff::LessThan,
                    PrefixDiff::PrefixOf(b_suffix) =>
                        node.right.prefix_cmp(&b_suffix, table),
                    PrefixDiff::Equal =>
                        PrefixDiff::PrefixedBy(node.right.clone()),
                    PrefixDiff::PrefixedBy(a_suffix) =>
                        PrefixDiff::PrefixedBy(Rc::new(a_suffix.merge(&node.right))),
                    PrefixDiff::GreaterThan => PrefixDiff::GreaterThan,
                }
            }
        };
        table.insert(my_hash, their_hash, result.clone());
        result
    }

    pub fn merge(self: &Rc<Self>, other: &Rc<Self>) -> Self {
        let hash = self.bromberg_hash() * other.bromberg_hash();
        Node::Internal(InternalNode {
            hash: hash,
            left: Rc::clone(self),
            right: Rc::clone(other),
        })
    }
}


#[derive(Clone)]
pub struct Mergle<T> {
    root: Rc<Node<T>>,
    table: MemTableRef<T>,
}

pub struct Iter<'a, T> {
    stack: Vec<(&'a InternalNode<T>, bool)>,
    tip: Option<&'a LeafNode<T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        panic!()
    }
}


impl<T: Ord + BrombergHashable> Mergle<T> {
    pub fn singleton(t: T, table: MemTableRef<T>) -> Mergle<T> {
        let h = t.bromberg_hash();
        let node = LeafNode {
            content: t,
            hash: h,
        };

        Mergle {
            root: Rc::new(Node::Leaf(node)),
            table: table,
        }
    }

    pub fn merge(&self, other: &Self) -> Self {
        Mergle {
            root: Rc::new(self.root.merge(&other.root)),
            table: self.table.clone(),
        }
    }

    fn iter(&self) -> Iter<T> {
        self.root.iter()
    }

    fn leaf_cmp(a: &LeafNode<T>, b: &LeafNode<T>) -> PrefixDiff<T> {
        match a.content.cmp(&b.content) {
            Ordering::Less => PrefixDiff::LessThan,
            Ordering::Equal => PrefixDiff::Equal,
            Ordering::Greater => PrefixDiff::GreaterThan,
        }
    }

    fn prefix_cmp(&self, other: &Self) -> PrefixDiff<T> {
        self.root.prefix_cmp(other.root.as_ref(), &self.table)
    }
}

impl<T: Ord + BrombergHashable> PartialEq for Mergle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<T: Ord + BrombergHashable> Eq for Mergle<T> {}

impl<T: Ord + BrombergHashable> PartialOrd for Mergle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T:  Ord + BrombergHashable> Ord for Mergle<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.prefix_cmp(other) {
            PrefixDiff::LessThan => Ordering::Less,
            PrefixDiff::PrefixOf(_) => Ordering::Less,
            PrefixDiff::Equal => Ordering::Equal,
            PrefixDiff::PrefixedBy(_) => Ordering::Greater,
            PrefixDiff::GreaterThan => Ordering::Greater,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::*;
    use rand::Rng;

    #[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
    struct U8(pub u8);

    impl BrombergHashable for U8 {
        fn bromberg_hash(&self) -> HashMatrix {
            hash(&[self.0])
        }
    }

    fn make_mergle(table: &MemTableRef<U8>, vec: &[u8]) -> Mergle<U8> {
        let mut rng = rand::thread_rng();
        if vec.len() == 0 {
            panic!()
        } else if vec.len() == 1 {
            Mergle::singleton(U8(vec[0]), table.clone())
        } else {
            let split = if vec.len() == 2 {
                1
            } else {
                rng.gen_range(1, vec.len() - 1)
            };
            let left = make_mergle(table, &vec[..split]);
            let right = make_mergle(table, &vec[split..]);
            left.merge(&right)
        }
    }

    quickcheck! {
        fn test_ord(a : Vec<u8>, b : Vec<u8>) -> TestResult {
            if a.is_empty() || b.is_empty() {
                return TestResult::discard();
            }
            let table : MemTableRef<U8> = MemTableRef::new();
            let a_mergle = make_mergle(&table, &a);
            let b_mergle = make_mergle(&table, &b);
            TestResult::from_bool((a_mergle.cmp(&b_mergle)) == (a.cmp(&b)))
        }
    }

    quickcheck! {
        fn test_eq(a : Vec<u8>) -> TestResult {
            if a.is_empty() {
                return TestResult::discard();
            }
            let table : MemTableRef<U8> = MemTableRef::new();
            let a_mergle = make_mergle(&table, &a);
            let b_mergle = make_mergle(&table, &a);
            TestResult::from_bool((a_mergle.cmp(&b_mergle)) == Ordering::Equal)
        }
    }
}
