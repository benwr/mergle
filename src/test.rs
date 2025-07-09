use crate::*;

use alloc::vec::Vec;

use bromberg_sl2::*;
use quickcheck::*;

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
struct U8(pub u8);

impl BrombergHashable for U8 {
    fn bromberg_hash(&self) -> HashMatrix {
        hash(&[self.0])
    }
}

impl Arbitrary for U8 {
    fn arbitrary(g: &mut Gen) -> Self {
        U8(u8::arbitrary(g))
    }
}

#[derive(Debug, Clone)]
enum MergleOp<T> {
    Singleton(T),
    Merge(usize, usize),
}

impl<T: Arbitrary> Arbitrary for MergleOp<T> {
    fn arbitrary(g: &mut Gen) -> Self {
        match bool::arbitrary(g) {
            false => MergleOp::Singleton(T::arbitrary(g)),
            true => MergleOp::Merge(usize::arbitrary(g), usize::arbitrary(g)),
        }
    }
}

fn make_mergle(vec: &[MergleOp<U8>]) -> Option<Mergle<U8>> {
    let mut mergles = Vec::new();
    let table = MemTableRef::new();
    for op in vec {
        let new_mergle = match op {
            MergleOp::Singleton(elem) => Some(Mergle::singleton(elem.clone(), &table)),
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
    fn test_ord(a : Vec<MergleOp<U8>>, b : Vec<MergleOp<U8>>) -> TestResult {
        match (make_mergle(&a), make_mergle(&b)) {
            (Some(a_mergle), Some(b_mergle)) => {
                let a_values : Vec<U8> = a_mergle.iter().collect();
                let b_values : Vec<U8> = b_mergle.iter().collect();
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

#[test]
fn ord_regression() {
    let a = vec![
        MergleOp::Singleton(U8(99)),
        MergleOp::Merge(28, 86),
        MergleOp::Merge(19, 13),
        MergleOp::Merge(66, 64),
    ];
    let b = vec![MergleOp::Singleton(U8(99)), MergleOp::Merge(13, 47)];

    if let (Some(a_mergle), Some(b_mergle)) = (make_mergle(&a), make_mergle(&b)) {
        let a_values: Vec<U8> = a_mergle.iter().collect();
        let b_values: Vec<U8> = b_mergle.iter().collect();
        let values_ord = a_values.cmp(&b_values);
        let mergle_ord = a_mergle.cmp(&b_mergle);
        assert_eq!(mergle_ord, values_ord);
    }
}

#[test]
fn ord_regression_2() {
    let a = vec![MergleOp::Singleton(U8(30))];
    let b = vec![MergleOp::Singleton(U8(39))];

    if let (Some(a_mergle), Some(b_mergle)) = (make_mergle(&a), make_mergle(&b)) {
        let a_values: Vec<U8> = a_mergle.iter().collect();
        let b_values: Vec<U8> = b_mergle.iter().collect();
        println!("{:?}", a_values);
        println!("{:?}", b_values);
        let values_ord = a_values.cmp(&b_values);
        let mergle_ord = a_mergle.cmp(&b_mergle);
        assert_eq!(mergle_ord, values_ord);
    }
}

quickcheck! {
    fn test_eq(a : Vec<MergleOp<U8>>) -> TestResult {
        match (make_mergle(&a), make_mergle(&a)) {
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
    fn test_pop(ops : Vec<MergleOp<U8>>) -> TestResult {
        match make_mergle(&ops) {
            Some(original) => {
                let mut orig_values : Vec<U8> = original.iter().collect();
                let orig_last = orig_values.pop().unwrap();
                let (last_elem, popped) = original.pop();
                let vecs_match = match popped {
                    Some(mergle) => mergle.iter().collect::<Vec<U8>>() == orig_values,
                    None => orig_values.is_empty()
                };
                TestResult::from_bool(vecs_match && last_elem == orig_last)
            },
            _ => {
                TestResult::discard()
            }
        }
    }
}
