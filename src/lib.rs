//! A simple range set implementation
//!
//! Once it's finished it should work with any Ord types
//!
//! Look Ma, no clones

use std::cmp::{Ord, Ordering};

/// Our range type is designed to use the minimum information needed
/// to fully describe the range. We even special-case one-element ranges.
///
/// Ranges have a lower and upper bound, see the Cut type for this.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Range<T> {
    /// An empty range
    Empty,
    /// A range including exactly one element, behaves like Closed(T, T)
    Singleton(T),
    /// A range between two bounds, excluding both
    Open(T, T),
    /// A range between two bounds, including the lower and excluding the upper
    ClosedOpen(T, T),
    /// A range between two bounds, excluding the lower and including the upper
    OpenClosed(T, T),
    /// A range between two bounds, including both
    Closed(T, T),
    /// A range with only a lower bound, excluded
    LeftOpen(T),
    /// A range with only a left bound,
    RightOpen(T),
    LeftClosed(T),
    RightClosed(T),
    Full,
}

/// A 'Cut' describes a lower or upper bound. As we want to avoid copies, it
/// borrows into the range where necessary.
/// Note that this is quite similar to collections::Bound, but the latter takes
/// the value instead of borrowing and has no None variant.
#[derive(Debug, PartialEq, Eq)]
pub enum Cut<'a, T: 'a> {
    /// A bound including the value
    Included(&'a T),
    /// A bound excluding the value
    Excluded(&'a T),
    /// An infinite bound
    Unbounded,
    /// No bound, because the Range is empty
    None,
}

impl<T> Range<T> {
    /// get the lower bound
    fn lower<'a>(&'a self) -> Cut<'a, T> {
        match *self {
            Range::Empty => Cut::None,
            Range::Singleton(ref l) |
            Range::ClosedOpen(ref l, _) |
            Range::Closed(ref l, _) |
            Range::LeftClosed(ref l) => Cut::Included(l),
            Range::Open(ref l, _) |
            Range::OpenClosed(ref l, _) |
            Range::LeftOpen(ref l) => Cut::Excluded(l),
            Range::RightOpen(_) |
            Range::RightClosed(_) |
            Range::Full => Cut::Unbounded,
        }
    }

    /// get the upper bound
    fn upper<'a>(&'a self) -> Cut<'a, T> {
        match *self {
            Range::Empty => Cut::None,
            Range::Singleton(ref l) |
            Range::OpenClosed(ref l, _) |
            Range::Closed(ref l, _) |
            Range::RightClosed(ref l) => Cut::Included(l),
            Range::Open(ref l, _) |
            Range::ClosedOpen(ref l, _) |
            Range::RightOpen(ref l) => Cut::Excluded(l),
            Range::LeftOpen(_) |
            Range::LeftClosed(_) |
            Range::Full => Cut::Unbounded,
        }
    }
}

pub enum UnionResult<T> {
    /// a union if the ranges touch
    Union(Range<T>),
    /// the original ranges if they don't touch
    Disjoint(Range<T>, Range<T>),
}

impl<T: Ord> Range<T> {
    pub fn contains(&self, value: &T) -> bool {
        match self.lower() {
            Cut::None => return false,
            Cut::Included(l) if value < l => return false,
            Cut::Excluded(l) if value <= l => return false,
            _ => ()
        }
        match self.upper() {
            Cut::None => false,
            Cut::Included(u) if value > u => false,
            Cut::Excluded(u) if value >= u => false,
            _ => true
        }
    }

    /// Does this range overlap with the other (self ∩ other ≠ ∅)?
    pub fn overlaps(&self, other: &Range<T>) -> bool {
        match (self.lower(), other.upper()) {
            (Cut::None, _) | (_, Cut::None) => return false,
            (Cut::Included(sl), Cut::Included(ou)) if sl > ou => return false,
            (Cut::Included(sl), Cut::Excluded(ou)) if sl >= ou => return false,
            (Cut::Excluded(sl), Cut::Included(ou)) if sl >= ou => return false,
            (Cut::Excluded(sl), Cut::Excluded(ou)) if sl >= ou => return false,
            _ => ()
        }
        match (self.upper(), other.lower()) {
            (Cut::None, _) | (_, Cut::None) => false,
            (Cut::Included(sl), Cut::Included(ou)) if sl > ou => false,
            (Cut::Included(sl), Cut::Excluded(ou)) if sl >= ou => false,
            (Cut::Excluded(sl), Cut::Included(ou)) if sl >= ou => false,
            (Cut::Excluded(sl), Cut::Excluded(ou)) if sl >= ou => false,
            _ => true
        }
    }

    /// Does this range touch or overlap the other?
    pub fn touches(&self, other: &Range<T>) -> bool {
        match (self.lower(), other.upper()) {
            (Cut::None, _) | (_, Cut::None) => return false,
            (Cut::Included(sl), Cut::Included(ou)) if sl > ou => return false,
            (Cut::Included(sl), Cut::Excluded(ou)) if sl > ou => return false,
            (Cut::Excluded(sl), Cut::Included(ou)) if sl > ou => return false,
            (Cut::Excluded(sl), Cut::Excluded(ou)) if sl >= ou => return false,
            _ => ()
        }
        match (self.upper(), other.lower()) {
            (Cut::None, _) | (_, Cut::None) => false,
            (Cut::Included(sl), Cut::Included(ou)) if sl > ou => false,
            (Cut::Included(sl), Cut::Excluded(ou)) if sl > ou => false,
            (Cut::Excluded(sl), Cut::Included(ou)) if sl > ou => false,
            (Cut::Excluded(sl), Cut::Excluded(ou)) if sl >= ou => false,
            _ => true
        }
    }

    /// Does this range englose the other?
    pub fn encloses(&self, other: &Range<T>) -> bool {
        // it does if self.lower <= other.lower and self.upper >= other.upper
        match (self.lower(), other.lower()) {
            (Cut::None, _) => return false,
            (_, Cut::None) => return true,
            (Cut::Included(sl), Cut::Included(ou)) if sl > ou => return false,
            (Cut::Included(sl), Cut::Excluded(ou)) if sl > ou => return false,
            (Cut::Excluded(sl), Cut::Included(ou)) if sl >= ou => return false,
            (Cut::Excluded(sl), Cut::Excluded(ou)) if sl > ou => return false,
            _ => ()
        }
        match (self.upper(), other.upper()) {
            (Cut::None, _) => false,
            (_, Cut::None) => return true,
            (Cut::Included(sl), Cut::Included(ou)) if sl < ou => false,
            (Cut::Included(sl), Cut::Excluded(ou)) if sl < ou => false,
            (Cut::Excluded(sl), Cut::Included(ou)) if sl <= ou => false,
            (Cut::Excluded(sl), Cut::Excluded(ou)) if sl < ou => false,
            _ => true
        }
    }

    /// returns a union of both ranges if they touch, the original ranges
    /// otherwise
    pub fn union(self, other: Range<T>) -> UnionResult<T> {
        unimplemented!();
    }

    pub fn intersect(self, other: Range<T>) -> Range<T> {
        unimplemented!();
    }
}

impl<T: Ord> Ord for Range<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.lower(), other.lower()) {
            (Cut::None, Cut::None) |
            (Cut::Unbounded, Cut::Unbounded) => (),
            (Cut::None, _) | (_, Cut::Unbounded) => return Ordering::Greater,
            (_, Cut::None) | (Cut::Unbounded, _) => return Ordering::Less,
            (Cut::Included(sl), Cut::Included(ou)) |
            (Cut::Excluded(sl), Cut::Excluded(ou)) => 
                if sl != ou { return sl.cmp(ou) },
            (Cut::Included(sl), Cut::Excluded(ou)) =>
                return if sl > ou { Ordering::Greater } else { Ordering::Less },
            (Cut::Excluded(sl), Cut::Included(ou)) =>
                return if sl < ou { Ordering::Less } else { Ordering::Greater },
        }
        match (self.upper(), other.upper()) {
            (Cut::None, Cut::None) |
            (Cut::Unbounded, Cut::Unbounded) => Ordering::Equal,
            (Cut::None, _) | (_, Cut::Unbounded) => Ordering::Less,
            (_, Cut::None) | (Cut::Unbounded, _) => Ordering::Greater,
            (Cut::Included(sl), Cut::Included(ou)) |
            (Cut::Excluded(sl), Cut::Excluded(ou)) => sl.cmp(ou),
            (Cut::Included(sl), Cut::Excluded(ou)) =>
                if sl < ou { Ordering::Less } else { Ordering::Greater },
            (Cut::Excluded(sl), Cut::Included(ou)) =>
                if sl > ou { Ordering::Greater } else { Ordering::Less },
        }
    }
}

// TODO: If someone asks, I may implement PartialOrd for T: PartialOrd, too
impl<T:Ord> PartialOrd for Range<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
