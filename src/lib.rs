#![no_std]

#[cfg(test)]
#[macro_use]
extern crate std;

extern crate alloc;

#[cfg(test)]
mod test;

use core::cmp::Ordering;

use num_bigint::BigUint;

use bromberg_sl2::{BrombergHashable, HashMatrix, I};
use fingertrees::rc::FingerTree;
use fingertrees::measure::Measured;
use fingertrees::monoid::Monoid;

pub enum PrefixDiff<T> {
    LessThan,
    PrefixOf(T),
    Equal,
    PrefixedBy(T),
    GreaterThan,
}

#[derive(Clone)]
struct Annotation {
    hash: HashMatrix,
    size: BigUint,
}

impl Monoid for Annotation {
    fn unit() -> Self {
        Annotation{
            hash: I,
            size: BigUint::from(0_u8),
        }
    }

    fn join(&self, other: &Self) -> Self {
        Annotation{
            hash: self.hash * other.hash,
            size: &self.size + &other.size,
        }
    }
}

#[derive(Clone)]
struct Leaf<T>(T, HashMatrix);
impl<T: BrombergHashable + Clone> Measured for Leaf<T> {
    type Measure = Annotation;
    fn measure(&self) -> Self::Measure {
        Annotation{
            hash: self.1,
            size: BigUint::from(1_u8),
        }
    }
}

impl<T> Ord for Leaf<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.1.cmp(&other.1)
    }
}

impl<T> PartialOrd for Leaf<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> PartialEq for Leaf<T> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(&other) == Ordering::Equal
    }
}

impl<T> Eq for Leaf<T> {}

#[derive(Clone)]
pub struct Mergle<T: BrombergHashable + Clone> {
    tree: FingerTree<Leaf<T>>,
}

fn prefix_cmp_equals<T: BrombergHashable + Clone + Ord>(
    left: &FingerTree<Leaf<T>>,
    right: &FingerTree<Leaf<T>>
) -> Ordering {
    let left_anno = left.measure();
    if left_anno.hash == right.measure().hash {
        return Ordering::Equal
    }
    let size = left_anno.size;
    match (left.view_left(), right.view_left()) {
        (None, None) => Ordering::Equal,
        (Some((left_first, left_rest)), Some((right_first, right_rest))) => {
            match left_first.0.cmp(&right_first.0) {
                Ordering::Less => Ordering::Less,
                Ordering::Greater => Ordering::Greater,
                Ordering::Equal => {
                    let new_size = (size - 1_u8) / 2_u8;
                    let (left_left, left_right) = size_split(&left_rest, &new_size);
                    let (right_left, right_right) = size_split(&right_rest, &new_size);
                    debug_assert!(left_left.measure().size == right_left.measure().size);
                    match prefix_cmp_equals(&left_left, &right_left) {
                        Ordering::Less => Ordering::Less,
                        Ordering::Greater => Ordering::Greater,
                        Ordering::Equal => {
                            debug_assert!(left_right.measure().size == right_right.measure().size);
                            prefix_cmp_equals(&left_right, &right_right)
                        }
                    }
                },
            }
        },
        _ => panic!("prefix_cmp_equals on unequal-length trees"),
    }
}

fn size_split<T: BrombergHashable + Clone>(
    t: &FingerTree<Leaf<T>>,
    s: &BigUint,
) -> (FingerTree<Leaf<T>>, FingerTree<Leaf<T>>) {
    t.split(|m| m.size > *s)
}

fn prefix_cmp<T: BrombergHashable + Clone + Ord>(
    left: &FingerTree<Leaf<T>>,
    right: &FingerTree<Leaf<T>>
) -> PrefixDiff<FingerTree<Leaf<T>>> {
    let left_size = left.measure().size;
    let right_size = right.measure().size;
    match left_size.cmp(&right_size) {
        Ordering::Equal => match prefix_cmp_equals(left, right) {
                Ordering::Less => PrefixDiff::LessThan,
                Ordering::Equal => PrefixDiff::Equal,
                Ordering::Greater => PrefixDiff::GreaterThan,
        },
        Ordering::Less => {
            let (right_eq, right_suffix) = size_split(right, &left_size);
            debug_assert!(right_eq.measure().size == left_size);
            match prefix_cmp_equals(left, &right_eq) {
                Ordering::Less => PrefixDiff::LessThan,
                Ordering::Equal => PrefixDiff::PrefixOf(right_suffix),
                Ordering::Greater => PrefixDiff::GreaterThan,
            }
        }
        Ordering::Greater => {
            let (left_eq, left_suffix) = size_split(left, &right_size);
            debug_assert!(left_eq.measure().size == right_size);
            match prefix_cmp_equals(&left_eq, right) {
                Ordering::Less => PrefixDiff::LessThan,
                Ordering::Equal => PrefixDiff::PrefixedBy(left_suffix),
                Ordering::Greater => PrefixDiff::GreaterThan,
            }
        }
    }
}

impl<T: BrombergHashable + Clone> Mergle<T> {
    pub fn singleton(t: T) -> Mergle<T> {
        let h = t.bromberg_hash();
        Mergle {
            tree: FingerTree::new().push_right(Leaf(t, h))
        }
    }

    #[must_use]
    pub fn merge(&self, other: &Self) -> Self {
        Mergle {
            tree: self.tree.concat(&other.tree)
        }
    }

    pub fn iter(&self) -> impl Iterator<Item=T> + '_ {
        self.tree.iter().map(|l| l.0)
    }

    #[must_use]
    pub fn pop(&self) -> (T, Option<Mergle<T>>) {
        match self.tree.view_right() {
            Some((v, r)) => {
                if r.is_empty() {
                    (v.0, None)
                } else {
                    (v.0, Some(Mergle{ tree: r }))
                }
            },
            None => panic!("Attempt to pop from empty tree"),
        }
    }
}

impl<T: BrombergHashable + Clone + Ord> Mergle<T> {
    #[must_use]
    pub fn prefix_cmp(&self, other: &Self) -> PrefixDiff<Mergle<T>> {
        match prefix_cmp(&self.tree, &other.tree) {
            PrefixDiff::LessThan => PrefixDiff::LessThan,
            PrefixDiff::PrefixOf(t) => PrefixDiff::PrefixOf(Mergle{tree: t}),
            PrefixDiff::Equal => PrefixDiff::Equal,
            PrefixDiff::PrefixedBy(t) => PrefixDiff::PrefixedBy(Mergle{tree: t}),
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
