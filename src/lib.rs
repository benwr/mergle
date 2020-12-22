#![no_std]

#[cfg(test)]
#[macro_use]
extern crate std;

extern crate alloc;

#[cfg(test)]
mod test;

use alloc::collections::btree_map::BTreeMap;
use alloc::rc::Rc;
use core::cell::RefCell;
use core::cmp::Ordering;

use bromberg_sl2::{BrombergHashable, HashMatrix, I};

pub struct MemTableRef(Rc<RefCell<BTreeMap<(HashMatrix, HashMatrix), Ordering>>>);

impl Clone for MemTableRef {
    fn clone(&self) -> Self {
        MemTableRef(Rc::clone(&self.0))
    }
}

impl MemTableRef {
    pub fn new() -> Self {
        MemTableRef(Rc::new(RefCell::new(BTreeMap::new())))
    }

    fn insert(&self, a: HashMatrix, b: HashMatrix, r: Ordering) {
        let mut table = self.0.borrow_mut();
        if a > b {
            table.insert((b, a), r.reverse());
        } else {
            table.insert((a, b), r.clone());
        }
    }

    fn lookup(&self, a: HashMatrix, b: HashMatrix) -> Option<Ordering> {
        if a == b {
            Some(Ordering::Equal)
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

#[derive(Clone)]
struct MergleNode<T> {
    elem: T,
    elem_hash: HashMatrix,
    height: usize,
    hash: HashMatrix,
    left: Option<Rc<MergleNode<T>>>,
    right: Option<Rc<MergleNode<T>>>,
}

impl<T: BrombergHashable + Clone> MergleNode<T> {
    fn singleton(e: T) -> Self {
        let h = e.bromberg_hash();
        MergleNode {
            elem: e,
            elem_hash: h,
            height: 1,
            hash: h,
            left: None,
            right: None,
        }
    }

    fn replace_left(&self, subtree: Option<Rc<Self>>) -> Self {
        MergleNode {
            elem: self.elem.clone(),
            elem_hash: self.elem_hash,
            height: usize::max(Self::height(&subtree), Self::height(&self.right)) + 1,
            hash: Self::hash(&subtree) * self.elem_hash * Self::hash(&self.right),
            left: subtree,
            right: self.right.clone(),
        }
    }

    fn replace_right(&self, subtree: Option<Rc<Self>>) -> Self {
        MergleNode {
            elem: self.elem.clone(),
            elem_hash: self.elem_hash,
            height: usize::max(Self::height(&self.left), Self::height(&subtree)) + 1,
            hash: Self::hash(&self.left) * self.elem_hash * Self::hash(&subtree),
            left: self.left.clone(),
            right: subtree,
        }
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

    fn rebalance(&self) -> Self {
        let b = self.balance();
        // five cases:
        if isize::abs(b) < 2 {
            // 1. We're balanced; nothing to do.
            self.clone()
        } else if b >= 2 {
            // left is too heavy.
            let left = self.left.as_ref().unwrap();
            if left.balance() < 0 {
                // 2. left is too heavy and right-leaning
                self.replace_left(Some(Rc::new(left.rotate_left()))).rotate_right()
            } else {
                // 3. left is too heavy and left-leaning or balanced
                self.rotate_right()
            }
        } else {
            let right = self.right.as_ref().unwrap();
            // right is too heavy.
            if right.balance() > 0 {
                // 2. right is too heavy and left-leaning
                self.replace_right(Some(Rc::new(right.rotate_right()))).rotate_left()
            } else {
                // 3. right is too heavy and right-leaning or balanced
                self.rotate_left()
            }
        }
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

    fn join_left_with_insert(left: &Rc<Self>, insertion: T, right: &Rc<Self>) -> Self {
        panic!()
    }

    fn join_right_with_insert(left: &Rc<Self>, insertion: T, right: &Rc<Self>) -> Self {
        panic!()
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
            MergleNode {
                elem_hash: elem_hash,
                elem: insertion,
                height: usize::max(left.height, right.height) + 1,
                hash: left.hash * elem_hash * right.hash,
                left: Some(left.clone()),
                right: Some(right.clone()),
            }
        }
    }

    fn join(left: &Rc<Self>, right: &Rc<Self>) -> Self {
        match left.pop_right() {
            (v, None) => right.push_left(v),
            (v, Some(new_left)) => Self::join_with_insert(&new_left, v, right)
        }
    }
}


impl<T: BrombergHashable> BrombergHashable for MergleNode<T> {
    fn bromberg_hash(&self) -> HashMatrix {
        self.hash
    }
}

pub struct Mergle<T> {
    root: Rc<MergleNode<T>>,
    table: MemTableRef,
}

impl<T: BrombergHashable + Clone> Mergle<T> {
    pub fn singleton(t: T, table: &MemTableRef) -> Mergle<T> {
        Mergle {
            root: Rc::new(MergleNode::singleton(t)),
            table: table.clone()
        }
    }

    #[must_use]
    pub fn merge(&self, other: &Self) -> Self {
        Mergle {
            root: Rc::new(MergleNode::join(&self.root, &other.root)),
            table: self.table.clone()
        }
    }

    /*
    pub fn iter(&self) -> impl Iterator<Item=T> + '_ {
        panic!()
    }
    */

    #[must_use]
    pub fn pop(&self) -> (T, Option<Mergle<T>>) {
        let (v, n) = self.root.pop_right();
        (v, n.map(|r| Mergle{root: r, table: self.table.clone()}))
    }
}

impl<T: BrombergHashable + Clone + Ord> Mergle<T> {
    #[must_use]
    pub fn prefix_cmp(&self, other: &Self) -> PrefixDiff<Mergle<T>> {
        panic!()
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
