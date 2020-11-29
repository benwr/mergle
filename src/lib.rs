use std::cmp::Ordering;
use std::collections::hash_map::HashMap;

use std::cell::RefCell;
use std::rc::Rc;

use bromberg_sl2::*;

enum PrefixResult<T> {
    LessThan,
    PrefixOf(Rc<MergleNode<T>>),
    Equal,
    PrefixedBy(Rc<MergleNode<T>>),
    GreaterThan,
}

impl<T> PrefixResult<T> {
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

impl<T> Clone for PrefixResult<T> {
    fn clone(&self) -> Self {
        match self {
            PrefixResult::PrefixOf(suffix) => PrefixResult::PrefixedBy(suffix.clone()),
            PrefixResult::PrefixedBy(suffix) => PrefixResult::PrefixOf(suffix.clone()),
            PrefixResult::LessThan => PrefixResult::LessThan,
            PrefixResult::Equal => PrefixResult::Equal,
            PrefixResult::GreaterThan => PrefixResult::GreaterThan,
        }
    }
}

pub struct MemoizationTableRef<T>(Rc<RefCell<HashMap<(HashMatrix, HashMatrix), PrefixResult<T>>>>);

impl<T> Clone for MemoizationTableRef<T> {
    fn clone(&self) -> Self {
        MemoizationTableRef(Rc::clone(&self.0))
    }
}

impl<T> MemoizationTableRef<T> {
    pub fn new() -> Self {
        MemoizationTableRef(Rc::new(RefCell::new(HashMap::new())))
    }

    fn insert(&self, a: HashMatrix, b: HashMatrix, r: PrefixResult<T>) {
        let mut table = self.0.borrow_mut();
        if a > b {
            table.insert((b, a), r.inverse());
        } else {
            table.insert((a, b), r.clone());
        }
    }

    fn lookup(&self, a: HashMatrix, b: HashMatrix) -> Option<PrefixResult<T>> {
        if a == b {
            Some(PrefixResult::Equal)
        } else {
            let table = self.0.borrow();
            if a > b {
                table.get(&(b, a)).map(|r| r.inverse())
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

    fn prefix_cmp(&self, other: &Self, table: &MemoizationTableRef<T>) -> PrefixResult<T> {
        let my_hash = self.bromberg_hash();
        let their_hash = other.bromberg_hash();
        if let Some(result) = table.lookup(my_hash, their_hash) {
            return result;
        }

        let result = match self {
            MergleNode::Leaf(leaf) => match other {
                MergleNode::Leaf(other_leaf) => Mergle::leaf_cmp(&leaf, &other_leaf),
                _ => other.prefix_cmp(self, table).inverse(),
            },
            MergleNode::Internal(node) => {
                match node.left.prefix_cmp(other, table) {
                    PrefixResult::LessThan => PrefixResult::LessThan,
                    PrefixResult::PrefixOf(b_suffix) => node.right.prefix_cmp(&b_suffix, table),
                    PrefixResult::Equal => PrefixResult::PrefixedBy(node.right.clone()),
                    PrefixResult::PrefixedBy(a_suffix) => {
                        PrefixResult::PrefixedBy(Rc::new(a_suffix.merge(&node.right)))
                    }
                    PrefixResult::GreaterThan => PrefixResult::GreaterThan,
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

    fn leaf_cmp(a: &MergleLeaf<T>, b: &MergleLeaf<T>) -> PrefixResult<T> {
        match a.content.cmp(&b.content) {
            Ordering::Less => PrefixResult::LessThan,
            Ordering::Equal => PrefixResult::Equal,
            Ordering::Greater => PrefixResult::GreaterThan,
        }
    }

    fn prefix_cmp(&self, other: &Self) -> PrefixResult<T> {
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
