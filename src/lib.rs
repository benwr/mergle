use std::cmp::Ordering;
use std::collections::hash_map::HashMap;

use std::cell::RefCell;
use std::rc::Rc;

use bromberg_sl2::*;

pub enum PrefixDiff<T> {
    LessThan,
    PrefixOf(T),
    Equal,
    PrefixedBy(T),
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

impl<T: Clone> Clone for PrefixDiff<T> {
    fn clone(&self) -> Self {
        match self {
            PrefixDiff::PrefixOf(suffix) => PrefixDiff::PrefixOf(suffix.clone()),
            PrefixDiff::PrefixedBy(suffix) => PrefixDiff::PrefixedBy(suffix.clone()),
            PrefixDiff::LessThan => PrefixDiff::LessThan,
            PrefixDiff::Equal => PrefixDiff::Equal,
            PrefixDiff::GreaterThan => PrefixDiff::GreaterThan,
        }
    }
}

pub struct MemTableRef<T>(Rc<RefCell<HashMap<(HashMatrix, HashMatrix), PrefixDiff<Rc<Node<T>>>>>>);

impl<T> Clone for MemTableRef<T> {
    fn clone(&self) -> Self {
        MemTableRef(Rc::clone(&self.0))
    }
}

impl<T> MemTableRef<T> {
    pub fn new() -> Self {
        MemTableRef(Rc::new(RefCell::new(HashMap::new())))
    }

    fn insert(&self, a: HashMatrix, b: HashMatrix, r: PrefixDiff<Rc<Node<T>>>) {
        let mut table = self.0.borrow_mut();
        if a > b {
            table.insert((b, a), r.inverse());
        } else {
            table.insert((a, b), r.clone());
        }
    }

    fn lookup(&self, a: HashMatrix, b: HashMatrix) -> Option<PrefixDiff<Rc<Node<T>>>> {
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
    fn iter<'a>(&'a self) -> Iter<'a, T> {
        let mut stack = vec![];
        let p: &Node<T> = self;
        let l = find_leftmost_leaf(p, &mut stack);
        return Iter {
            stack: stack,
            tip: Some(l),
        };
    }

    fn prefix_cmp(&self, other: &Self, table: &MemTableRef<T>) -> PrefixDiff<Rc<Node<T>>> {
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
            (Node::Leaf(leaf), Node::Leaf(other_leaf)) => Mergle::leaf_cmp(&leaf, &other_leaf),
            (Node::Leaf(_), _) => other.prefix_cmp(self, table).inverse(),
            (Node::Internal(node), _) => match node.left.prefix_cmp(other, table) {
                PrefixDiff::LessThan => PrefixDiff::LessThan,
                PrefixDiff::PrefixOf(b_suffix) => node.right.prefix_cmp(&b_suffix, table),
                PrefixDiff::Equal => PrefixDiff::PrefixedBy(node.right.clone()),
                PrefixDiff::PrefixedBy(a_suffix) => {
                    PrefixDiff::PrefixedBy(Rc::new(a_suffix.merge(&node.right)))
                }
                PrefixDiff::GreaterThan => PrefixDiff::GreaterThan,
            },
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

    pub fn build_leaf(t: T) -> Self {
        let h = t.bromberg_hash();
        let node = LeafNode {
            content: t,
            hash: h,
        };
        Node::Leaf(node)
    }

    pub fn modify_right(&self, t: T) -> Rc<Self> {
        let new_node = match self {
            Node::Leaf(_) => Node::build_leaf(t),
            Node::Internal(n) => {
                let new_right = n.right.modify_right(t);
                Node::merge(&n.left, &new_right)
            }
        };
        Rc::new(new_node)
    }
}

#[derive(Clone)]
pub struct Mergle<T> {
    root: Rc<Node<T>>,
    table: MemTableRef<T>,
}

fn find_leftmost_leaf<'a, T>(
    mut p: &'a Node<T>,
    stack: &mut Vec<(&'a InternalNode<T>, bool)>,
) -> &'a LeafNode<T> {
    loop {
        match p {
            Node::Internal(n) => {
                stack.push((n, false));
                p = &*n.left;
            }
            Node::Leaf(n) => {
                return n;
            }
        }
    }
}
pub struct Iter<'a, T> {
    stack: Vec<(&'a InternalNode<T>, bool)>,
    tip: Option<&'a LeafNode<T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let result = self.tip.map(|t| &t.content);

        self.tip = None;

        while let Some((n, b)) = self.stack.pop() {
            if !b {
                self.stack.push((n, true));
                let l = find_leftmost_leaf(&*n.right, &mut self.stack);
                self.tip = Some(l);
                break;
            }
        }
        result
    }
}

impl<T: Ord + BrombergHashable> Mergle<T> {
    pub fn singleton(t: T, table: MemTableRef<T>) -> Mergle<T> {
        Mergle {
            root: Rc::new(Node::build_leaf(t)),
            table: table,
        }
    }

    pub fn merge(&self, other: &Self) -> Self {
        Mergle {
            root: Rc::new(self.root.merge(&other.root)),
            table: self.table.clone(),
        }
    }

    pub fn append(&self, t: T) -> Mergle<T> {
        let elem = Mergle::singleton(t, self.table.clone());
        self.merge(&elem)
    }

    pub fn prepend(&self, t: T) -> Mergle<T> {
        let elem = Mergle::singleton(t, self.table.clone());
        elem.merge(self)
    }

    pub fn iter(&self) -> Iter<T> {
        self.root.iter()
    }

    fn leaf_cmp(a: &LeafNode<T>, b: &LeafNode<T>) -> PrefixDiff<Rc<Node<T>>> {
        match a.content.cmp(&b.content) {
            Ordering::Less => PrefixDiff::LessThan,
            Ordering::Equal => PrefixDiff::Equal,
            Ordering::Greater => PrefixDiff::GreaterThan,
        }
    }

    fn prefix_cmp(&self, other: &Self) -> PrefixDiff<Rc<Node<T>>> {
        self.root.prefix_cmp(other.root.as_ref(), &self.table)
    }

    pub fn prefix_diff(&self, other: &Self) -> PrefixDiff<Mergle<T>> {
        match self.root.prefix_cmp(other.root.as_ref(), &self.table) {
            PrefixDiff::LessThan => PrefixDiff::LessThan,
            PrefixDiff::PrefixOf(suffix) => PrefixDiff::PrefixOf(Mergle {
                root: suffix.clone(),
                table: self.table.clone(),
            }),
            PrefixDiff::Equal => PrefixDiff::Equal,
            PrefixDiff::PrefixedBy(suffix) => PrefixDiff::PrefixedBy(Mergle {
                root: suffix.clone(),
                table: self.table.clone(),
            }),
            PrefixDiff::GreaterThan => PrefixDiff::GreaterThan,
        }
    }

    pub fn modify_last(&self, t: T) -> Mergle<T> {
        Mergle {
            root: self.root.modify_right(t),
            table: self.table.clone(),
        }
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

impl<T: Ord + BrombergHashable> Ord for Mergle<T> {
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

    #[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
    struct U8(pub u8);

    impl BrombergHashable for U8 {
        fn bromberg_hash(&self) -> HashMatrix {
            hash(&[self.0])
        }
    }

    impl Arbitrary for U8 {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            U8(u8::arbitrary(g))
        }
    }

    #[derive(Debug, Clone)]
    enum MergleOp {
        Singleton(U8),
        Merge(usize, usize),
    }

    impl Arbitrary for MergleOp {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            match bool::arbitrary(g) {
                false => MergleOp::Singleton(U8(u8::arbitrary(g))),
                true => MergleOp::Merge(usize::arbitrary(g), usize::arbitrary(g)),
            }
        }
    }

    fn make_mergle(table: &MemTableRef<U8>, vec: &[MergleOp]) -> Option<Mergle<U8>> {
        let mut mergles = Vec::new();
        for op in vec {
            let new_mergle = match op {
                MergleOp::Singleton(elem) => Some(Mergle::singleton(elem.clone(), table.clone())),
                MergleOp::Merge(a, b) => {
                    if !mergles.is_empty() {
                        let mergle_a: &Mergle<U8> = mergles.get(a % mergles.len()).unwrap();
                        let mergle_b: &Mergle<U8> = mergles.get(b % mergles.len()).unwrap();
                        Some(mergle_a.merge(&mergle_b))
                    } else {
                        None
                    }
                }
            };
            if let Some(m) = new_mergle {
                mergles.push(m);
            }
        }
        mergles.pop()
    }

    quickcheck! {
        fn test_ord(a : Vec<MergleOp>, b : Vec<MergleOp>) -> TestResult {
            let table : MemTableRef<U8> = MemTableRef::new();
            match (make_mergle(&table, &a), make_mergle(&table, &b)) {
                (Some(a_mergle), Some(b_mergle)) => {
                    let a_values : Vec<&U8> = a_mergle.iter().collect();
                    let b_values : Vec<&U8> = b_mergle.iter().collect();
                    let values_ord = a_values.cmp(&b_values);
                    let mergle_ord = a_mergle.cmp(&b_mergle);
                    TestResult::from_bool(mergle_ord == values_ord)
                },
                _ => {
                    TestResult::discard()
                }
            }
        }
    }

    quickcheck! {
        fn test_append(a : Vec<MergleOp>, elem : U8) -> TestResult {
            let table : MemTableRef<U8> = MemTableRef::new();
            match make_mergle(&table, &a) {
                Some(a_mergle) => {
                    let mut a_values : Vec<&U8> = a_mergle.iter().collect();
                    a_values.push(&elem);
                    let appended = a_mergle.append(elem.clone());
                    TestResult::from_bool(appended.iter().collect::<Vec<&U8>>() == a_values)
                },
                _ => {
                    TestResult::discard()
                }
            }
        }
    }

    quickcheck! {
        fn test_prepend(a : Vec<MergleOp>, elem : U8) -> TestResult {
            let table : MemTableRef<U8> = MemTableRef::new();
            match make_mergle(&table, &a) {
                Some(a_mergle) => {
                    let mut a_values : Vec<&U8> = a_mergle.iter().collect();
                    a_values.insert(0, &elem);
                    let prepended = a_mergle.prepend(elem.clone());
                    TestResult::from_bool(prepended.iter().collect::<Vec<&U8>>() == a_values)
                },
                _ => {
                    TestResult::discard()
                }
            }
        }
    }

    #[test]
    fn ord_regression() {
        let a = vec![
            MergleOp::Singleton(U8(99)),
            MergleOp::Merge(28, 86),
            MergleOp::Merge(19, 13),
            MergleOp::Merge(66, 64),
        ];
        let b = vec![MergleOp::Singleton(U8(99)), MergleOp::Merge(13, 47)];

        let table: MemTableRef<U8> = MemTableRef::new();
        if let (Some(a_mergle), Some(b_mergle)) = (make_mergle(&table, &a), make_mergle(&table, &b))
        {
            let a_values: Vec<&U8> = a_mergle.iter().collect();
            let b_values: Vec<&U8> = b_mergle.iter().collect();
            let values_ord = a_values.cmp(&b_values);
            let mergle_ord = a_mergle.cmp(&b_mergle);
            assert_eq!(mergle_ord, values_ord);
        }
    }

    quickcheck! {
        fn test_eq(a : Vec<MergleOp>) -> TestResult {
            let table : MemTableRef<U8> = MemTableRef::new();
            match (make_mergle(&table, &a), make_mergle(&table, &a)) {
                (Some(a_mergle), Some(b_mergle)) => {
                    TestResult::from_bool(a_mergle == b_mergle)
                },
                _ => {
                    TestResult::discard()
                }
            }
        }
    }

    quickcheck! {
        fn test_modify_last(ops : Vec<MergleOp>, elem : U8) -> TestResult {
            let table : MemTableRef<U8> = MemTableRef::new();
            match make_mergle(&table, &ops) {
                Some(original) => {
                    let last_elem = original.iter().last().unwrap();
                    if last_elem == &elem {
                        return TestResult::discard();
                    }

                    let modified = original.modify_last(elem.clone());
                    let mut orig_values : Vec<&U8> = original.iter().collect();
                    let orig_last = orig_values.pop().unwrap();
                    let mut modified_values : Vec<&U8> = modified.iter().collect();
                    let mod_last = modified_values.pop().unwrap();
                    TestResult::from_bool(orig_values == modified_values && orig_last == last_elem && mod_last == &elem)
                },
                _ => {
                    TestResult::discard()
                }
            }
        }
    }
}
