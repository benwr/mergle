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

struct MergleNode<T> {
    elem: T,
    elem_hash: HashMatrix,
    height: usize,
    hash: HashMatrix,
    left: Option<Rc<MergleNode<T>>>,
    right: Option<Rc<MergleNode<T>>>,
}

impl<T: BrombergHashable + Clone> MergleNode<T> {
    fn singleton(e: T) -> MergleNode<T> {
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

    fn height(t: &Option<Rc<MergleNode<T>>>) -> usize {
        match t {
            None => 0,
            Some(p) => p.height,
        }
    }

    fn hash(t: &Option<Rc<MergleNode<T>>>) -> HashMatrix {
        match t {
            None => I,
            Some(p) => p.hash,
        }
    }

    fn rotate_left(&self) -> MergleNode<T> {
        let right: &Rc<MergleNode<T>> = self.right.as_ref().unwrap();

        let left_height = 1 + usize::max(
            Self::height(&self.left),
            Self::height(&right.left));
        let total_height = usize::max(1 + left_height, right.height);

        MergleNode {
            hash: self.hash,
            height: total_height,
            elem: right.elem.clone(),
            elem_hash: right.elem_hash,
            left: Some(Rc::new(MergleNode {
                hash: Self::hash(&self.left) * self.elem_hash * Self::hash(&right.left),
                height: left_height,
                elem: self.elem.clone(),
                elem_hash: self.elem_hash,
                left: self.left.clone(),
                right: right.left.clone(),
            })),
            right: right.right.clone(),
        }
    }


    fn rotate_right(&self) -> MergleNode<T> {
        let left: &Rc<MergleNode<T>> = self.left.as_ref().unwrap();

        let right_height = 1 + usize::max(
            Self::height(&self.right),
            Self::height(&left.right));
        let total_height = usize::max(left.height, 1 + right_height);

        MergleNode {
            hash: self.hash,
            height: total_height,
            elem: left.elem.clone(),
            elem_hash: left.elem_hash,
            left: left.left.clone(),
            right: Some(Rc::new(MergleNode {
                hash: Self::hash(&left.right) * self.elem_hash * Self::hash(&self.right),
                height: right_height,
                elem: self.elem.clone(),
                elem_hash: self.elem_hash,
                left: self.right.clone(),
                right: left.right.clone(),
            })),
        }
    }

    fn join(left: &MergleNode<T>, right: &MergleNode<T>) -> MergleNode<T> {
        panic!()
    }
}

impl<T: BrombergHashable> BrombergHashable for MergleNode<T> {
    fn bromberg_hash(&self) -> HashMatrix {
        self.hash
    }
}

pub struct Mergle<T> {
    root: Rc<MergleNode<T>>,
}

impl<T: BrombergHashable + Clone> Mergle<T> {
    pub fn singleton(t: T, table: &MemTableRef) -> Mergle<T> {
        panic!()
    }

    #[must_use]
    pub fn merge(&self, other: &Self) -> Self {
        panic!()
    }

    /*
    pub fn iter(&self) -> impl Iterator<Item=T> + '_ {
        panic!()
    }
    */

    #[must_use]
    pub fn pop(&self) -> (T, Option<Mergle<T>>) {
        panic!()
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
