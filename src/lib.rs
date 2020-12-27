#![no_std]

#[cfg(test)]
#[macro_use]
extern crate std;

extern crate alloc;

#[cfg(test)]
mod test;

use alloc::collections::btree_map::BTreeMap;
use alloc::rc::Rc;
use alloc::vec::Vec;
use core::cell::RefCell;
use core::cmp::Ordering;

use bromberg_sl2::{BrombergHashable, HashMatrix, I};

pub struct MemTableRef<T>(
    Rc<RefCell<BTreeMap<(HashMatrix, HashMatrix), PrefixDiff<Rc<MergleNode<T>>>>>>,
);

impl<T> Clone for MemTableRef<T> {
    fn clone(&self) -> Self {
        MemTableRef(Rc::clone(&self.0))
    }
}

impl<T: Clone> MemTableRef<T> {
    pub fn new() -> Self {
        MemTableRef(Rc::new(RefCell::new(BTreeMap::new())))
    }

    fn insert(&self, a: HashMatrix, b: HashMatrix, r: PrefixDiff<Rc<MergleNode<T>>>) {
        let mut table = self.0.borrow_mut();
        if a > b {
            table.insert((b, a), r.reverse());
        } else {
            table.insert((a, b), r.clone());
        }
    }

    fn lookup(&self, a: HashMatrix, b: HashMatrix) -> Option<PrefixDiff<Rc<MergleNode<T>>>> {
        if a == b {
            Some(PrefixDiff::Equal)
        } else {
            let table = self.0.borrow();
            if a > b {
                table.get(&(b, a)).map(|r| r.reverse())
            } else {
                table.get(&(a, b)).map(|r| r.clone())
            }
        }
    }
}

#[derive(Clone)]
pub enum PrefixDiff<T> {
    LessThan,
    PrefixOf(T),
    Equal,
    PrefixedBy(T),
    GreaterThan,
}

impl<T: Clone> PrefixDiff<T> {
    fn reverse(&self) -> Self {
        match self {
            PrefixDiff::LessThan => PrefixDiff::GreaterThan,
            PrefixDiff::PrefixOf(t) => PrefixDiff::PrefixedBy(t.clone()),
            PrefixDiff::Equal => PrefixDiff::Equal,
            PrefixDiff::PrefixedBy(t) => PrefixDiff::PrefixOf(t.clone()),
            PrefixDiff::GreaterThan => PrefixDiff::LessThan,
        }
    }

    fn from_ord(o: Ordering) -> Self {
        match o {
            Ordering::Less => PrefixDiff::LessThan,
            Ordering::Equal => PrefixDiff::Equal,
            Ordering::Greater => PrefixDiff::GreaterThan,
        }
    }
}

#[derive(Clone)]
pub struct MergleNode<T> {
    elem: T,
    elem_hash: HashMatrix,
    height: usize,
    hash: HashMatrix,
    left: Option<Rc<MergleNode<T>>>,
    right: Option<Rc<MergleNode<T>>>,
}

impl<T: BrombergHashable + Ord + Clone> MergleNode<T> {
    fn new(
        elem: T,
        elem_hash: HashMatrix,
        left: Option<Rc<Self>>,
        right: Option<Rc<Self>>,
    ) -> Self {
        MergleNode {
            elem: elem,
            elem_hash: elem_hash,
            height: usize::max(Self::height(&left), Self::height(&right)) + 1,
            hash: Self::hash(&left) * elem_hash * Self::hash(&right),
            left: left,
            right: right,
        }
    }

    fn singleton(e: T) -> Self {
        let h = e.bromberg_hash();
        Self::new(e, h, None, None)
    }

    fn replace_left(&self, subtree: Option<Rc<Self>>) -> Self {
        Self::new(
            self.elem.clone(),
            self.elem_hash,
            subtree,
            self.right.clone(),
        )
    }

    fn replace_right(&self, subtree: Option<Rc<Self>>) -> Self {
        Self::new(
            self.elem.clone(),
            self.elem_hash,
            self.left.clone(),
            subtree,
        )
    }

    fn height(t: &Option<Rc<Self>>) -> usize {
        match t {
            None => 0,
            Some(p) => p.height,
        }
    }

    fn balance(&self) -> isize {
        (Self::height(&self.left) as isize) - (Self::height(&self.right) as isize)
    }

    fn hash(t: &Option<Rc<Self>>) -> HashMatrix {
        match t {
            None => I,
            Some(p) => p.hash,
        }
    }

    fn rotate_left(&self) -> Self {
        let right = self.right.as_ref().unwrap();
        right.replace_left(Some(Rc::new(self.replace_right(right.left.clone()))))
    }

    fn rotate_right(&self) -> Self {
        let left = self.left.as_ref().unwrap();
        left.replace_right(Some(Rc::new(self.replace_left(left.right.clone()))))
    }

    fn rebalance(self) -> Self {
        let b = self.balance();
        let res = if b > 1 {
            // left is too heavy.
            let left = self.left.as_ref().unwrap();
            if left.balance() < 0 {
                self.replace_left(Some(Rc::new(left.rotate_left())))
                    .rotate_right()
            } else {
                self.rotate_right()
            }
        } else if b < -1 {
            let right = self.right.as_ref().unwrap();
            // right is too heavy.
            if right.balance() > 0 {
                self.replace_right(Some(Rc::new(right.rotate_right())))
                    .rotate_left()
            } else {
                self.rotate_left()
            }
        } else {
            self.clone()
        };
        debug_assert!(isize::abs(Self::balance(&res)) < 2);
        res
    }

    fn pop_right(&self) -> (T, Option<Rc<Self>>) {
        match &self.right {
            None => (self.elem.clone(), self.left.clone()),
            Some(t) => {
                let (v, r_res) = t.pop_right();
                let candidate_res = self.replace_right(r_res);
                (v, Some(Rc::new(candidate_res.rebalance())))
            }
        }
    }

    fn push_left(&self, insertion: T) -> Self {
        let left = match &self.left {
            None => Self::singleton(insertion),
            Some(t) => t.push_left(insertion),
        };
        self.replace_left(Some(Rc::new(left))).rebalance()
    }

    fn join_left_with_insert(left: &Rc<Self>, t: T, right: &Rc<Self>) -> Self {
        let t_prime = if Self::height(&right.left) > left.height + 1 {
            Self::join_left_with_insert(left, t, right.left.as_ref().unwrap())
        } else {
            let h = t.bromberg_hash();
            Self::new(t, h, Some(left.clone()), right.left.clone())
        };
        right.replace_left(Some(Rc::new(t_prime))).rebalance()
    }

    fn join_right_with_insert(left: &Rc<Self>, t: T, right: &Rc<Self>) -> Self {
        let t_prime = if Self::height(&left.right) > right.height + 1 {
            Self::join_right_with_insert(left.right.as_ref().unwrap(), t, right)
        } else {
            let h = t.bromberg_hash();
            Self::new(t, h, left.right.clone(), Some(right.clone()))
        };

        left.replace_right(Some(Rc::new(t_prime))).rebalance()
    }

    fn join_with_insert(left: &Rc<Self>, insertion: T, right: &Rc<Self>) -> Self {
        let balance = (left.height as isize) - (right.height as isize);
        if balance > 1 {
            // left-weighted
            Self::join_right_with_insert(left, insertion, right)
        } else if balance < -1 {
            // right-weighted
            Self::join_left_with_insert(left, insertion, right)
        } else {
            let elem_hash = insertion.bromberg_hash();
            Self::new(
                insertion,
                elem_hash,
                Some(left.clone()),
                Some(right.clone()),
            )
        }
    }

    fn join(left: &Rc<Self>, right: &Rc<Self>) -> Self {
        match left.pop_right() {
            (v, None) => right.push_left(v),
            (v, Some(new_left)) => Self::join_with_insert(&new_left, v, right),
        }
    }

    fn elem_plus_right(&self) -> Self {
        match &self.right {
            None => MergleNode::singleton(self.elem.clone()),
            Some(r) => r.push_left(self.elem.clone()),
        }
    }

    fn prefix_diff(self: &Self, other: &Self, table: &MemTableRef<T>) -> PrefixDiff<Rc<Self>> {
        if let Some(res) = table.lookup(self.hash, other.hash) {
            return res;
        }
        if self.hash == other.hash {
            return PrefixDiff::Equal;
        }

        let res = match (self.height, other.height) {
            (1, 1) => PrefixDiff::from_ord(self.elem.cmp(&other.elem)),
            (a, b) if a >= b => {
                let left_subtree = self.left.as_ref().unwrap();
                match left_subtree.prefix_diff(other, table) {
                    PrefixDiff::LessThan => PrefixDiff::LessThan,
                    PrefixDiff::PrefixOf(b_suffix) => {
                        MergleNode::prefix_diff(&self.elem_plus_right(), &b_suffix, table)
                    }
                    PrefixDiff::Equal => PrefixDiff::PrefixedBy(Rc::new(self.elem_plus_right())),
                    PrefixDiff::PrefixedBy(a_suffix) => PrefixDiff::PrefixedBy(Rc::new(
                        MergleNode::join(&a_suffix, &Rc::new(self.elem_plus_right())),
                    )),
                    PrefixDiff::GreaterThan => PrefixDiff::GreaterThan,
                }
            }
            (_, _) => MergleNode::prefix_diff(other, self, table).reverse(),
        };

        table.insert(self.hash, other.hash, res.clone());
        res
    }
}

impl<T: BrombergHashable> BrombergHashable for MergleNode<T> {
    fn bromberg_hash(&self) -> HashMatrix {
        self.hash
    }
}

#[derive(Clone)]
pub struct Mergle<T> {
    root: Rc<MergleNode<T>>,
    table: MemTableRef<T>,
}

impl<T: BrombergHashable + Clone + Ord> Mergle<T> {
    pub fn singleton(t: T, table: &MemTableRef<T>) -> Mergle<T> {
        Mergle {
            root: Rc::new(MergleNode::singleton(t)),
            table: table.clone(),
        }
    }

    #[must_use]
    pub fn merge(&self, other: &Self) -> Self {
        Mergle {
            root: Rc::new(MergleNode::join(&self.root, &other.root)),
            table: self.table.clone(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = T> + '_ {
        let mut stack = Vec::new();
        stack.push(self.root.clone());
        Iter {
            stack: stack,
        }
    }

    #[must_use]
    pub fn pop(&self) -> (T, Option<Mergle<T>>) {
        let (v, n) = self.root.pop_right();
        (
            v,
            n.map(|r| Mergle {
                root: r,
                table: self.table.clone(),
            }),
        )
    }
}

impl<T: BrombergHashable + Clone + Ord> Mergle<T> {
    #[must_use]
    pub fn prefix_cmp(&self, other: &Self) -> PrefixDiff<Mergle<T>> {
        match self.root.prefix_diff(&*other.root, &self.table) {
            PrefixDiff::LessThan => PrefixDiff::LessThan,
            PrefixDiff::PrefixOf(node) => PrefixDiff::PrefixOf(Mergle {
                root: node.clone(),
                table: self.table.clone(),
            }),
            PrefixDiff::Equal => PrefixDiff::Equal,
            PrefixDiff::PrefixedBy(node) => PrefixDiff::PrefixedBy(Mergle {
                root: node.clone(),
                table: self.table.clone(),
            }),
            PrefixDiff::GreaterThan => PrefixDiff::GreaterThan,
        }
    }
}

impl<T: BrombergHashable + Clone + Ord> Ord for Mergle<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.prefix_cmp(other) {
            PrefixDiff::LessThan | PrefixDiff::PrefixOf(_) => Ordering::Less,
            PrefixDiff::Equal => Ordering::Equal,
            PrefixDiff::PrefixedBy(_) | PrefixDiff::GreaterThan => Ordering::Greater,
        }
    }
}

impl<T: BrombergHashable + Clone + Ord> PartialOrd for Mergle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: BrombergHashable + Clone + Ord> PartialEq for Mergle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<T: BrombergHashable + Clone + Ord> Eq for Mergle<T> {}

pub struct Iter<T> {
    stack: Vec<Rc<MergleNode<T>>>,
}

impl<T: BrombergHashable + Clone + Ord> Iterator for Iter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let mut result = None;
        while let Some(node) = self.stack.pop() {
            if let Some(right_tree) = &node.right {
                self.stack.push(right_tree.clone());
            };

            if let Some(left_tree) = &node.left {
                self.stack
                    .push(Rc::new(MergleNode::singleton(node.elem.clone())));
                self.stack.push(left_tree.clone());
            } else {
                result = Some(node.elem.clone());
                break;
            }
        }
        result
    }
}
