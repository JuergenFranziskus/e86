pub enum RelocationMode {
    WordALU,
    /// Used by the eight-byte load and store instructions (0xC3, 0xC4).
    /// The constant's bits are ored into the ranges 8, 28 thru 63.
    WordMemory,
    /// Used by the eight-byte lea instruction (0xC5).
    /// The constant's bits are ored into the ranges 8, 24 thru 25, 28 thru 63.
    WordLea,
}
