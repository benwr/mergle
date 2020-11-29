use std::cmp::Ordering;
use std::collections::hash_map::HashMap;

use std::cell::RefCell;
use std::rc::Rc;

use bromberg_sl2::*;

enum PrefixOrdering<T> {
    LessThan,
    PrefixOf(Rc<MergleNode<T>>),
    Equal,
    PrefixedBy(Rc<MergleNode<T>>),
    GreaterThan,
}

impl<T> PrefixOrdering<T> {
    fn inverse(self) -> PrefixOrdering<T> {
        match self {
            PrefixOrdering::LessThan => PrefixOrdering::GreaterThan,
            PrefixOrdering::PrefixOf(suffix) => PrefixOrdering::PrefixedBy(suffix),
            PrefixOrdering::Equal => PrefixOrdering::Equal,
            PrefixOrdering::PrefixedBy(suffix) => PrefixOrdering::PrefixOf(suffix),
            PrefixOrdering::GreaterThan => PrefixOrdering::LessThan,
        }
    }
}

impl<T> Clone for PrefixOrdering<T> {
    fn clone(&self) -> Self {
        match self {
            PrefixOrdering::PrefixOf(suffix) => PrefixOrdering::PrefixedBy(suffix.clone()),
            PrefixOrdering::PrefixedBy(suffix) => PrefixOrdering::PrefixOf(suffix.clone()),
            PrefixOrdering::LessThan => PrefixOrdering::LessThan,
            PrefixOrdering::Equal => PrefixOrdering::Equal,
            PrefixOrdering::GreaterThan => PrefixOrdering::GreaterThan,
        }
    }
}

pub struct MemoizationTableRef<T>(Rc<RefCell<HashMap<(HashMatrix, HashMatrix), PrefixOrdering<T>>>>);

impl<T> Clone for MemoizationTableRef<T> {
    fn clone(&self) -> Self {
        MemoizationTableRef(Rc::clone(&self.0))
    }
}

impl<T> MemoizationTableRef<T> {
    pub fn new() -> Self {
        MemoizationTableRef(Rc::new(RefCell::new(HashMap::new())))
    }

    fn insert(&self, a: HashMatrix, b: HashMatrix, r: PrefixOrdering<T>) {
        let mut table = self.0.borrow_mut();
        if a > b {
            table.insert((b, a), r.inverse());
        } else {
            table.insert((a, b), r.clone());
        }
    }

    fn lookup(&self, a: HashMatrix, b: HashMatrix) -> Option<PrefixOrdering<T>> {
        if a == b {
            Some(PrefixOrdering::Equal)
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

struct MergleInternalNode<T> {
    hash: HashMatrix,
    left: Rc<MergleNode<T>>,
    right: Rc<MergleNode<T>>,
}

struct MergleLeaf<T> {
    content: T,
    hash: HashMatrix,
}

enum MergleNode<T> {
    Internal(MergleInternalNode<T>),
    Leaf(MergleLeaf<T>),
}

impl<T> BrombergHashable for MergleNode<T> {
    fn bromberg_hash(&self) -> HashMatrix {
        match self {
            MergleNode::Internal(node) => node.hash,
            MergleNode::Leaf(node) => node.hash,
        }
    }
}

impl<T: Ord + BrombergHashable> MergleNode<T> {
    fn values(&self) -> Vec<&T> {
        match self {
            MergleNode::Internal(node) => [node.left.values(), node.right.values()].concat(),
            MergleNode::Leaf(leaf) => vec![&leaf.content],
        }
    }

    fn prefix_cmp(&self, other: &Self, table: &MemoizationTableRef<T>) -> PrefixOrdering<T> {
        let my_hash = self.bromberg_hash();
        let their_hash = other.bromberg_hash();
        if let Some(result) = table.lookup(my_hash, their_hash) {
            return result;
        }
        // TODO perhaps this should recurse in a more principled way. I'm not sure
        // what exactly that way would be, but it might make use of e.g. nodes tracking
        // ther left-height, or maybe their order statistics (though order statistics
        // will grow really fast and make the asymptotics bad, probably).
        let result = match self {
            MergleNode::Leaf(leaf) => match other {
                MergleNode::Leaf(other_leaf) => Mergle::leaf_cmp(&leaf, &other_leaf),
                _ => other.prefix_cmp(self, table).inverse(),
            },
            MergleNode::Internal(node) => {
                match node.left.prefix_cmp(other, table) {
                    PrefixOrdering::LessThan => PrefixOrdering::LessThan,
                    PrefixOrdering::PrefixOf(b_suffix) => node.right.prefix_cmp(&b_suffix, table),
                    PrefixOrdering::Equal => PrefixOrdering::PrefixedBy(node.right.clone()),
                    PrefixOrdering::PrefixedBy(a_suffix) =>
                        PrefixOrdering::PrefixedBy(Rc::new(a_suffix.merge(&node.right))),
                    PrefixOrdering::GreaterThan => PrefixOrdering::GreaterThan,
                }
            }
        };
        table.insert(my_hash, their_hash, result.clone());
        result
    }

    pub fn merge(self: &Rc<Self>, other: &Rc<Self>) -> Self {
        let hash = self.bromberg_hash() * other.bromberg_hash();
        MergleNode::Internal(MergleInternalNode {
            hash: hash,
            left: Rc::clone(self),
            right: Rc::clone(other),
        })
    }
}


#[derive(Clone)]
pub struct Mergle<T> {
    root: Rc<MergleNode<T>>,
    table: MemoizationTableRef<T>,
}


impl<T: Ord + BrombergHashable> Mergle<T> {
    pub fn singleton(t: T, table: MemoizationTableRef<T>) -> Mergle<T> {
        let h = t.bromberg_hash();
        let node = MergleLeaf {
            content: t,
            hash: h,
        };

        Mergle {
            root: Rc::new(MergleNode::Leaf(node)),
            table: table,
        }
    }

    pub fn merge(&self, other: &Self) -> Self {
        Mergle {
            root: Rc::new(self.root.merge(&other.root)),
            table: self.table.clone(),
        }
    }

    fn values(&self) -> Vec<&T> {
        self.root.values()
    }

    fn leaf_cmp(a: &MergleLeaf<T>, b: &MergleLeaf<T>) -> PrefixOrdering<T> {
        match a.content.cmp(&b.content) {
            Ordering::Less => PrefixOrdering::LessThan,
            Ordering::Equal => PrefixOrdering::Equal,
            Ordering::Greater => PrefixOrdering::GreaterThan,
        }
    }

    fn prefix_cmp(&self, other: &Self) -> PrefixOrdering<T> {
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
            PrefixOrdering::LessThan => Ordering::Less,
            PrefixOrdering::PrefixOf(_) => Ordering::Less,
            PrefixOrdering::Equal => Ordering::Equal,
            PrefixOrdering::PrefixedBy(_) => Ordering::Greater,
            PrefixOrdering::GreaterThan => Ordering::Greater,
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

    fn make_mergle(table: &MemoizationTableRef<U8>, vec: &[u8]) -> Mergle<U8> {
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
            let table : MemoizationTableRef<U8> = MemoizationTableRef::new();
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
            let table : MemoizationTableRef<U8> = MemoizationTableRef::new();
            let a_mergle = make_mergle(&table, &a);
            let b_mergle = make_mergle(&table, &a);
            TestResult::from_bool((a_mergle.cmp(&b_mergle)) == Ordering::Equal)
        }
    }
}
