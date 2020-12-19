#![no_std]

#[macro_use]
extern crate alloc;

use core::cmp::Ordering;

use im_rc::OrdSet;
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

#[derive(Clone)]
struct Annotation<T>(HashMatrix, BigUint, OrdSet<Leaf<T>>);
impl<T: Clone> Monoid for Annotation<T> {
    fn unit() -> Self {
        Annotation(I, BigUint::from(0_u8), OrdSet::new())
    }

    fn join(&self, other: &Self) -> Self {
        Annotation(
            self.0 * other.0,
            &self.1 + &other.1,
            OrdSet::unions(vec![self.2.clone(), other.2.clone()])
        )
    }
}

#[derive(Clone)]
struct Leaf<T>(T, HashMatrix);
impl<T: BrombergHashable + Clone> Measured for Leaf<T> {
    type Measure = Annotation<T>;
    fn measure(&self) -> Self::Measure {
        Annotation(self.1, BigUint::from(1_u8), OrdSet::unit(self.clone()))
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
        self.1.cmp(&other.1) == Ordering::Equal
    }
}

impl<T> Eq for Leaf<T> {}

pub struct Mergle<T: BrombergHashable + Clone> {
    tree: FingerTree<Leaf<T>>,
}

fn prefix_diff_equals<T: BrombergHashable + Clone>(
    left: &FingerTree<Leaf<T>>,
    right: &FingerTree<Leaf<T>>
) -> PrefixDiff<FingerTree<Leaf<T>>> {
    panic!();
}

fn size_split<T: BrombergHashable + Clone>(
    t: &FingerTree<Leaf<T>>,
    s: BigUint,
) -> (FingerTree<Leaf<T>>, FingerTree<Leaf<T>>) {
    t.split(|m| m.1 <= s)
}

fn prefix_diff<T: BrombergHashable + Clone>(
    left: &FingerTree<Leaf<T>>,
    right: &FingerTree<Leaf<T>>
) -> PrefixDiff<FingerTree<Leaf<T>>> {
    let left_size = left.measure().1;
    let right_size = left.measure().1;
    match left_size.cmp(&right_size) {
        Ordering::Equal => prefix_diff_equals(left, right),
        Ordering::Less => {
            let (right_eq, right_suffix) = size_split(right, left_size);
            match prefix_diff_equals(left, &right_eq) {
                PrefixDiff::LessThan => PrefixDiff::LessThan,
                PrefixDiff::Equal => PrefixDiff::PrefixOf(right_suffix),
                PrefixDiff::GreaterThan => PrefixDiff::GreaterThan,
                _ => panic!("one equal-length sequence seems to be a prefix of another?")
            }
        }
        Ordering::Greater => {
            let (left_eq, left_suffix) = size_split(left, right_size);
            match prefix_diff_equals(&left_eq, right) {
                PrefixDiff::LessThan => PrefixDiff::LessThan,
                PrefixDiff::Equal => PrefixDiff::PrefixedBy(left_suffix),
                PrefixDiff::GreaterThan => PrefixDiff::GreaterThan,
                _ => panic!("one equal-length sequence seems to be a prefix of another?")
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

    pub fn unique_elems(&self) -> impl Iterator<Item=T> + '_ {
        self.tree.measure().2.into_iter().map(|l| l.0)
    }

    #[must_use]
    pub fn prefix_diff(&self, other: &Self) -> PrefixDiff<Mergle<T>> {
        match prefix_diff(&self.tree, &other.tree) {
            PrefixDiff::LessThan => PrefixDiff::LessThan,
            PrefixDiff::PrefixOf(t) => PrefixDiff::PrefixOf(Mergle{tree: t}),
            PrefixDiff::Equal => PrefixDiff::Equal,
            PrefixDiff::PrefixedBy(t) => PrefixDiff::PrefixedBy(Mergle{tree: t}),
            PrefixDiff::GreaterThan => PrefixDiff::GreaterThan,
        }
    }
}

impl<T: BrombergHashable + Clone> Ord for Mergle<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.prefix_diff(other) {
            PrefixDiff::LessThan | PrefixDiff::PrefixOf(_) => Ordering::Less,
            PrefixDiff::Equal => Ordering::Equal,
            PrefixDiff::PrefixedBy(_) | PrefixDiff::GreaterThan => Ordering::Greater,
        }
    }
}

impl<T: BrombergHashable + Clone> PartialOrd for Mergle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: BrombergHashable + Clone> PartialEq for Mergle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<T: BrombergHashable + Clone> Eq for Mergle<T> {}
