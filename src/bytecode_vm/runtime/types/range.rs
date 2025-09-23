use std::fmt::{Display, Error, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub struct Range {
    pub start: i64,
    pub stop: i64,
    pub step: i64,
}

impl Range {
    const DEFAULT_START: i64 = 0;
    const DEFAULT_STEP: i64 = 1;

    pub fn new(start: i64, stop: i64, step: i64) -> Self {
        Self { start, stop, step }
    }

    pub fn with_stop(stop: i64) -> Self {
        Self::new(Self::DEFAULT_START, stop, Self::DEFAULT_STEP)
    }

    pub fn with_start_stop(start: i64, stop: i64) -> Self {
        Self::new(start, stop, Self::DEFAULT_STEP)
    }

    // Do not take ownership so we can reuse this Range if we like.
    pub fn iter(&self) -> RangeIter {
        RangeIter {
            current: self.start,
            stop: self.stop,
            step: self.step,
        }
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if self.step == 1 {
            write!(f, "range({}, {})", self.start, self.stop)
        } else {
            write!(f, "range({}, {}, {})", self.start, self.stop, self.step)
        }
    }
}

#[derive(Clone, Debug)]
pub struct RangeIter {
    current: i64,
    stop: i64,
    step: i64,
}

impl Iterator for RangeIter {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        if (self.step > 0 && self.current >= self.stop)
            || (self.step < 0 && self.current <= self.stop)
        {
            None
        } else {
            let result = self.current;
            self.current += self.step;
            Some(result)
        }
    }
}
