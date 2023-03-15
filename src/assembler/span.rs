


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub length: usize,
}
impl Span {
    pub fn new(start: usize, length: usize) -> Self {
        Self {
            start,
            length
        }
    }

    pub fn merge(self, other: Span) -> Span {
        let start = self.start.min(other.start);
        let end = self.end().max(other.end());
        let length = end - start;
        Self::new(start, length)
    }
    pub fn end(self) -> usize {
        self.start + self.length
    }
}
