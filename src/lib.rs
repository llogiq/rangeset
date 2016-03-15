//! A simple range set implementation
//!
//! Once it's finished it should work with any Ord types
//!
//! Look Ma, no clones

use std::cmp::{Ord, Ordering, max, min};

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
    /// A range with only an upper bound, excluded
    RightOpen(T),
    /// A range with only a lower bound, included
    LeftClosed(T),
    /// A range with only an upper bound, included
    RightClosed(T),
    /// A range containing all values
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
    pub fn lower<'a>(&'a self) -> Cut<'a, T> {
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
    pub fn upper<'a>(&'a self) -> Cut<'a, T> {
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

    fn canonicalize(self) -> Range<T> {
        match self {
            Range::Closed(l, r) =>
                match l.cmp(&r) {
                    Ordering::Less => Range::Closed(l, r),
                    Ordering::Equal => Range::Singleton(l),
                    Ordering::Greater => Range::Empty
                },
            Range::OpenClosed(l, r) =>
                if l < r { Range::OpenClosed(l, r) } else { Range::Empty },
            Range::ClosedOpen(l, r) =>
                if l < r { Range::ClosedOpen(l, r) } else { Range::Empty },
            Range::Open(l, r) =>
                if l < r { Range::Open(l, r) } else { Range::Empty },
            x => x
        }
    }

    pub fn intersect(self, other: Range<T>) -> Range<T> {
        let r = match (self, other) {
            (Range::Empty, _) | (_, Range::Empty) => Range::Empty,
            (Range::Full, x) | (x, Range::Full) => x,
            (Range::Singleton(s), other) | (other, Range::Singleton(s)) =>
                if other.contains(&s) { Range::Singleton(s) } else { Range::Empty },

            (Range::LeftClosed(sl), Range::LeftClosed(ol)) => Range::LeftClosed(max(sl, ol)),
            (Range::LeftClosed(sl), Range::LeftOpen(ol)) =>
                if ol < sl { Range::LeftOpen(ol) } else { Range::LeftClosed(sl) },
            (Range::LeftClosed(sl), Range::RightClosed(ou)) => Range::Closed(sl, ou),
            (Range::LeftClosed(sl), Range::RightOpen(ou)) => Range::ClosedOpen(sl, ou),
            (Range::LeftClosed(sl), Range::ClosedOpen(ol, ou)) =>
                Range::ClosedOpen(max(sl, ol), ou),
            (Range::LeftClosed(sl), Range::Closed(ol, ou)) => Range::Closed(max(sl, ol), ou),
            (Range::LeftClosed(sl), Range::OpenClosed(ol, ou)) =>
                if ol < sl { Range::OpenClosed(ol, ou) } else { Range::Closed(sl, ou) },
            (Range::LeftClosed(sl), Range::Open(ol, ou)) =>
                if ol < sl { Range::Open(ol, ou) } else { Range::ClosedOpen(sl, ou) },

            (Range::LeftOpen(sl), Range::LeftClosed(ol)) =>
                if sl < ol { Range::LeftOpen(sl) } else { Range::LeftClosed(ol) },
            (Range::LeftOpen(sl), Range::LeftOpen(ol)) => Range::LeftOpen(max(sl, ol)),
            (Range::LeftOpen(sl), Range::RightClosed(ou)) => Range::OpenClosed(sl, ou),
            (Range::LeftOpen(sl), Range::RightOpen(ou)) => Range::Open(sl, ou),
            (Range::LeftOpen(sl), Range::ClosedOpen(ol, ou)) =>
                if sl < ol { Range::Open(sl, ou) } else { Range::ClosedOpen(ol, ou) },
            (Range::LeftOpen(sl), Range::Closed(ol, ou)) =>
                if sl < ol { Range::OpenClosed(sl, ou) } else { Range::Closed(ol, ou) },
            (Range::LeftOpen(sl), Range::OpenClosed(ol, ou)) => Range::OpenClosed(max(sl, ol), ou),
            (Range::LeftOpen(sl), Range::Open(ol, ou)) => Range::Open(max(sl, ol), ou),

            (Range::RightClosed(su), Range::LeftClosed(ol)) => Range::Closed(ol, su),
            (Range::RightClosed(su), Range::LeftOpen(ol)) => Range::ClosedOpen(ol, su),
            (Range::RightClosed(su), Range::RightClosed(ou)) => Range::RightClosed(min(su, ou)),
            (Range::RightClosed(su), Range::RightOpen(ou)) =>
                if su < ou { Range::RightClosed(su) } else { Range::RightOpen(ou) },
            (Range::RightClosed(su), Range::ClosedOpen(ol, ou)) =>
                if su < ou { Range::Closed(ol, su) } else { Range::ClosedOpen(ol, ou) },
            (Range::RightClosed(su), Range::Closed(ol, ou)) => Range::Closed(ol, min(su, ou)),
            (Range::RightClosed(su), Range::OpenClosed(ol, ou)) => Range::OpenClosed(ol, min(su, ou)),
            (Range::RightClosed(su), Range::Open(ol, ou)) =>
                if su < ou { Range::OpenClosed(ol, su) } else { Range::Open(ol, ou) },

            (Range::RightOpen(su), Range::LeftClosed(ol)) => Range::ClosedOpen(ol, su),
            (Range::RightOpen(su), Range::LeftOpen(ol)) => Range::Open(ol, su),
            (Range::RightOpen(su), Range::RightClosed(ou)) =>
                if ou < su { Range::RightClosed(ou) } else { Range::RightOpen(su) },
            (Range::RightOpen(su), Range::RightOpen(ou)) => Range::RightOpen(min(su, ou)),
            (Range::RightOpen(su), Range::ClosedOpen(ol, ou)) => Range::ClosedOpen(ol, min(su, ou)),
            (Range::RightOpen(su), Range::Closed(ol, ou)) =>
                if ou < su { Range::Closed(ol, ou) } else { Range::ClosedOpen(ol, su) },
            (Range::RightOpen(su), Range::OpenClosed(ol, ou)) =>
                if ou < su { Range::OpenClosed(ol, ou) } else { Range::Open(ol, su) },
            (Range::RightOpen(su), Range::Open(ol, ou)) => Range::Open(ol, min(su, ou)),

            (Range::ClosedOpen(sl, su), Range::LeftClosed(ol)) => Range::ClosedOpen(max(sl, ol), su),
            (Range::ClosedOpen(sl, su), Range::LeftOpen(ol)) =>
                if sl > ol { Range::ClosedOpen(sl, su) } else { Range::Open(ol, su) },
            (Range::ClosedOpen(sl, su), Range::RightClosed(ou)) =>
                if ou < su { Range::Closed(sl, ou) } else { Range::ClosedOpen(sl, su) },
            (Range::ClosedOpen(sl, su), Range::RightOpen(ou)) => Range::ClosedOpen(sl, min(su, ou)),
            (Range::ClosedOpen(sl, su), Range::ClosedOpen(ol, ou)) =>
                Range::ClosedOpen(max(sl, ol), min(su, ou)),
            (Range::ClosedOpen(sl, su), Range::Closed(ol, ou)) => {
                let l = max(sl, ol);
                if ou > su { Range::ClosedOpen(l, su) } else { Range::Closed(l, ou) }
            }
            (Range::ClosedOpen(sl, su), Range::OpenClosed(ol, ou)) =>
                if sl > ol {
                    if ou < su { Range::Closed(sl, ou) } else { Range::ClosedOpen(sl, su) }
                } else if ou < su { Range::OpenClosed(ol, ou) } else { Range::Open(ol, su) },
            (Range::ClosedOpen(sl, su), Range::Open(ol, ou)) => {
                let u = min(su, ou);
                if sl > ol { Range::ClosedOpen(sl, u) } else { Range::Open(ol, u) }
            }

            (Range::Closed(sl, su), Range::LeftClosed(ol)) => Range::Closed(max(sl, ol), su),
            (Range::Closed(sl, su), Range::LeftOpen(ol)) =>
                if sl > ol { Range::Closed(sl, su) } else { Range::OpenClosed(ol, su) },
            (Range::Closed(sl, su), Range::RightClosed(ou)) => Range::Closed(sl, min(su, ou)),
            (Range::Closed(sl, su), Range::RightOpen(ou)) =>
                if su < ou { Range::Closed(sl, su) } else { Range::ClosedOpen(sl, ou) },
            (Range::Closed(sl, su), Range::ClosedOpen(ol, ou)) => {
                let l = max(sl, ol);
                if su < ou { Range::Closed(l, su) } else { Range::ClosedOpen(l, ou) }
            }
            (Range::Closed(sl, su), Range::Closed(ol, ou)) =>
                Range::Closed(max(sl, ol), min(su, ou)),
            (Range::Closed(sl, su), Range::OpenClosed(ol, ou)) => {
                let u = min(su, ou);
                if sl > ol { Range::Closed(sl, u) } else { Range::OpenClosed(ol, u) }
            }
            (Range::Closed(sl, su), Range::Open(ol, ou)) =>
                if sl > ol {
                    if su < ou { Range::Closed(sl, su) } else { Range::ClosedOpen(sl, ou) }
                } else if su < ou { Range::OpenClosed(ol, su) } else { Range::Open(ol, ou) },

            (Range::OpenClosed(sl, su), Range::LeftClosed(ol)) =>
                if ol > sl { Range::Closed(ol, su) } else { Range::OpenClosed(sl, su) },
            (Range::OpenClosed(sl, su), Range::LeftOpen(ol)) => Range::OpenClosed(max(sl, ol), su),
            (Range::OpenClosed(sl, su), Range::RightClosed(ou)) => Range::OpenClosed(sl, min(su, ou)),
            (Range::OpenClosed(sl, su), Range::RightOpen(ou)) =>
                if su < ou { Range::OpenClosed(sl, su) } else { Range::Open(sl, ou) },
            (Range::OpenClosed(sl, su), Range::ClosedOpen(ol, ou)) =>
                if sl < ol {
                    if su < ou { Range::Closed(ol, su) } else { Range::ClosedOpen(ol, ou) }
                } else if su < ou { Range::OpenClosed(sl, su) } else { Range::Open(sl, ou) },
            (Range::OpenClosed(sl, su), Range::Closed(ol, ou)) => {
                let u = min(su, ou);
                if sl < ol { Range::Closed(ol, u) } else { Range::OpenClosed(sl, u) }
            }
            (Range::OpenClosed(sl, su), Range::OpenClosed(ol, ou)) =>
                Range::OpenClosed(max(sl, ol), min(su, ou)),
            (Range::OpenClosed(sl, su), Range::Open(ol, ou)) => {
                let l = max(sl, ol);
                if su < ou { Range::OpenClosed(l, su) } else { Range::Open(l, ou) }
            }

            (Range::Open(sl, su), Range::LeftClosed(ol)) =>
                if ol > sl { Range::ClosedOpen(ol, su) } else { Range::Open(sl, su) },
            (Range::Open(sl, su), Range::LeftOpen(ol)) => Range::Open(max(sl, ol), su),
            (Range::Open(sl, su), Range::RightClosed(ou)) =>
                if ou < su { Range::OpenClosed(sl, ou) } else { Range::Open(sl, su) },
            (Range::Open(sl, su), Range::RightOpen(ou)) => Range::Open(sl, min(su, ou)),
            (Range::Open(sl, su), Range::ClosedOpen(ol, ou)) => {
                let u = min(su, ou);
                if ol > sl { Range::ClosedOpen(ol, u) } else { Range::Open(sl, u) }
            }
            (Range::Open(sl, su), Range::Closed(ol, ou)) =>
                if ol > sl {
                    if ou < su { Range::Closed(ol, ou) } else { Range::ClosedOpen(ol, su) }
                } else if ou < su { Range::OpenClosed(sl, ou) } else { Range::Open(sl, su) },
            (Range::Open(sl, su), Range::OpenClosed(ol, ou)) => {
                let l = max(sl, ol);
                if ou < su { Range::OpenClosed(l, ou) } else { Range::Open(l, su) }
            }
            (Range::Open(sl, su), Range::Open(ol, ou)) =>
                Range::Open(max(sl, ol), min(su, ou)),
        };
        r.canonicalize()
    }

    /// returns a union of both ranges if they touch, the original ranges
    /// otherwise
    pub fn union(self, other: Range<T>) -> UnionResult<T> {
        match(self, other) {
            (Range::Empty, r) | (r, Range::Empty) => Union(Range::Full),
            (Range::Full, _) | (_, Range::Full) => Union(Range::Full),

            (Range::Singleton(v), r) | (r, Range::Singleton) =>
                if r.contains(v) { Union(r) } else { Disjoint(Range::Singleton(v), r) },

            (Range::LeftClosed(sl), Range::LeftClosed(ol)) => Union(Range::LeftClosed(min(sl, ol))),
            (Range::LeftClosed(sl), Range::LeftOpen(ol)) =>
                Union(if ol < sl { Range::LeftOpen(ol) } else { Range::LeftClosed(sl) }),
            (Range::LeftClosed(sl), Range::RightClosed(ou)) => Range::Closed(sl, ou),
            (Range::LeftClosed(sl), Range::RightOpen(ou)) => Range::ClosedOpen(sl, ou),
            (Range::LeftClosed(sl), Range::ClosedOpen(ol, ou)) =>
                Range::ClosedOpen(max(sl, ol), ou),
            (Range::LeftClosed(sl), Range::Closed(ol, ou)) => Range::Closed(max(sl, ol), ou),
            (Range::LeftClosed(sl), Range::OpenClosed(ol, ou)) =>
                if ol < sl { Range::OpenClosed(ol, ou) } else { Range::Closed(sl, ou) },
            (Range::LeftClosed(sl), Range::Open(ol, ou)) =>
                if ol < sl { Range::Open(ol, ou) } else { Range::ClosedOpen(sl, ou) },

            _ => unimplemented!()
        }
    }
}

impl<T: Ord> Ord for Range<T> {
    // we compare by lower bound first, then by upper bound
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
