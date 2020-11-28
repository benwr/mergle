use std::cmp::Ordering;
use std::collections::hash_map::HashMap;

use std::cell::RefCell;
use std::rc::Rc;

use bromberg_sl2::*;

#[derive(Clone)]
pub enum PrefixResult<T> {
    LessThan,
    PrefixOf(Mergle<T>),
    Equal,
    PrefixedBy(Mergle<T>),
    GreaterThan,
}

impl<T: Clone + BrombergHashable> PrefixResult<T> {
    fn inverse(&self) -> PrefixResult<T> {
        match self {
            PrefixResult::LessThan => PrefixResult::GreaterThan,
            PrefixResult::PrefixOf(suffix) => PrefixResult::PrefixedBy(suffix.clone()),
            PrefixResult::Equal => PrefixResult::Equal,
            PrefixResult::PrefixedBy(suffix) => PrefixResult::PrefixOf(suffix.clone()),
            PrefixResult::GreaterThan => PrefixResult::LessThan,
        }
    }
}

pub type MemoizationTable<T> = HashMap<(HashMatrix, HashMatrix), PrefixResult<T>>;

#[derive(Clone)]
pub struct MemoizationTableRef<T>(Rc<RefCell<MemoizationTable<T>>>);

impl<T: Clone + BrombergHashable> MemoizationTableRef<T> {
    pub fn new() -> Self {
        MemoizationTableRef(Rc::new(RefCell::new(MemoizationTable::new())))
    }

    fn insert(&self, a: HashMatrix, b: HashMatrix, r: PrefixResult<T>) {
        let mut table = self.0.borrow_mut();
        table.insert((a, b), r.clone());
    }

    fn lookup(&self, a: HashMatrix, b: HashMatrix) -> Option<PrefixResult<T>> {
        if a == b {
            return Some(PrefixResult::Equal);
        }
        let table = self.0.borrow();
        if let Some(result) = table.get(&(a, b)) {
            return Some(result.clone());
        } else if let Some(result) = table.get(&(b, a)) {
            return Some(result.inverse());
        }
        None
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

impl<T> MergleNode<T> {
    fn values(&self) -> Vec<&T> {
        match self {
            MergleNode::Internal(node) => [node.left.values(), node.right.values()].concat(),
            MergleNode::Leaf(leaf) => vec![&leaf.content],
        }
    }
}

#[derive(Clone)]
pub struct Mergle<T> {
    root: Rc<MergleNode<T>>,
    table: MemoizationTableRef<T>,
}


impl<T: Clone + Ord + BrombergHashable> Mergle<T> {
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
        let hash = self.root.bromberg_hash() * other.root.bromberg_hash();
        let node = MergleInternalNode {
            hash: hash,
            left: Rc::clone(&self.root),
            right: Rc::clone(&other.root),
        };
        Mergle {
            root: Rc::new(MergleNode::Internal(node)),
            table: self.table.clone(),
        }
    }

    pub fn copy(&self) -> Self {
        Mergle {
            root: Rc::clone(&self.root),
            table: self.table.clone(),
        }
    }

    fn values(&self) -> Vec<&T> {
        self.root.values()
    }

    fn from_node(node: Rc<MergleNode<T>>, table: MemoizationTableRef<T>) -> Mergle<T> {
        Mergle {
            root: node,
            table: table,
        }
    }

    fn leaf_cmp(a: &MergleLeaf<T>, b: &MergleLeaf<T>) -> PrefixResult<T> {
        match a.content.cmp(&b.content) {
            Ordering::Less => PrefixResult::LessThan,
            Ordering::Equal => PrefixResult::Equal,
            Ordering::Greater => PrefixResult::GreaterThan,
        }
    }

    pub fn prefix_cmp(&self, other: &Self) -> PrefixResult<T> {
        let self_hash = self.root.bromberg_hash();
        let other_hash = other.root.bromberg_hash();
        if let Some(result) = self.table.lookup(self_hash, other_hash) {
            return result;
        }

        let result = match self.root.as_ref() {
            MergleNode::Leaf(leaf) => match other.root.as_ref() {
                MergleNode::Leaf(other_leaf) => Mergle::leaf_cmp(&leaf, &other_leaf),
                _ => other.prefix_cmp(self).inverse(),
            },
            MergleNode::Internal(node) => {
                let left_mergle = Mergle::from_node(node.left.clone(), self.table.clone());
                let right_mergle =
                    Mergle::from_node(node.right.clone(), self.table.clone());
                match left_mergle.prefix_cmp(other) {
                    PrefixResult::LessThan => PrefixResult::LessThan,
                    PrefixResult::PrefixOf(b_suffix) => right_mergle.prefix_cmp(&b_suffix),
                    PrefixResult::Equal => PrefixResult::PrefixedBy(right_mergle),
                    PrefixResult::PrefixedBy(a_suffix) => {
                        PrefixResult::PrefixedBy(a_suffix.merge(&right_mergle))
                    }
                    PrefixResult::GreaterThan => PrefixResult::GreaterThan,
                }
            }
        };

        result
    }
}

impl<T: Clone + Ord + BrombergHashable> PartialEq for Mergle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<T: Clone + Ord + BrombergHashable> Eq for Mergle<T> {}

impl<T: Clone + Ord + BrombergHashable> PartialOrd for Mergle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Clone + Ord + BrombergHashable> Ord for Mergle<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.prefix_cmp(other) {
            PrefixResult::LessThan => Ordering::Less,
            PrefixResult::PrefixOf(_) => Ordering::Less,
            PrefixResult::Equal => Ordering::Equal,
            PrefixResult::PrefixedBy(_) => Ordering::Greater,
            PrefixResult::GreaterThan => Ordering::Greater,
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
