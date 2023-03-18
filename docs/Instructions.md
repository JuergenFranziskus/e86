# Instruction Encoding
Instructions may be 1, 2, 4 or 8 bytes long.


## Single Byte Instructions
All single byte instructions must  
be of the form 0b0xxxxxx.

- 0b0_0000000: Nop (0x0)  
    Do nothing

- 0b0_00aaaaa: Set Zero (0x1)  
    Set register indicated by 'a' to zero.  
    The zero register is meaningless and not allowed.

- 0b0_01aaaaa: Load Flags (0x1)  
    Copy the value from the flags register into the 'a' register.
    The zero register is meaningless and not allowed.

- 0b0_10aaaaa: Store Flags (0x0)  
    Copy the value from the 'a' register into the flags register.


## Two Byte Instructions
The first (lowest) byte of two-byte instructions  
must have the form 0b10xxxxxx.

- 0b000bbbbb_10_000000: Test Negative (0x0)  
    Store 'b' < 0 into flags register.

- 0baaa00000_10_0000aa: Test Not Negative (0x0)  
    Store 'a' >= 0 into flags register.

- 0baaabbbbb_10_0000aa: Mov (0x1)  
    Copy the value from register 'b' into register 'a'.  
    The zero register is not allowed for both a and b.

- 0baaaccccc_10_0xxxaa: ALU Operation (0x2)  
    Perform an ALU Operation with A = 'a', B = 'c' and store  
    the result back into 'a'.
    'c' is a sign extended constant value.  
    A value of zero is not allowed for either 'a' or 'c'.  
    The operation is given by 'x' and maps as follows:
    * 0 -> Illegal
    * 1 -> (1, 0) And
    * 2 -> (1, 1) Or
    * 3 -> (1, 2) Xor
    * 4 -> (1, 3) Shl
    * 5 -> (1, 4) Shr
    * 6 -> (2, 0) Add
    * 7 -> (2, 1) Mul

- 0b000bbbbb_10_100000: Test Zero (0x0)  
    Perform ALU Test Zero Operation (3, 0) on register 'b'  
    and store the result in the flags register.  
    A value of zero is not allowed for 'b'.

- 0b000bbbbb_10_100100: Test Not Zero (0x0)  
    Perform ALU Test Not Zero Operation (3, 0) on register 'b'  
    and store the result in the flags register.  
    A value of zero is not allowed for 'a'.

- 0baaabbbbb_10_100xaa: Unary Operation (0x1)  
    Perform an ALU Operation of family 0 with A = 'b',  
    storing the result into 'a'.
    A value of zero is not allowed for 'a'.  
    The operation is given by 'x'.

- 0baaabbbbb_10_1010aa: Add (0x1)  
    Calculate 'a' + 'b' and store result in 'a'.  
    The zero register is not allowed for 'a' or 'b'.

- 0baaabbbbb_10_1011aa: Sub (0x1)  
    Calculate 'a' - 'b' and store result in 'a'.  
    The zero register is not allowed for 'a' or 'b'.

- 0baaabbbbb_10_1100aa: Mul (0x1)  
    Calculate 'a' * 'b' and store result in 'a'.  
    The zero register is not allowed for 'a' or 'b'.


- 0baaaccccc_10_1101aa: Mov Constant (0x2)  
    Move the sign extended constant value 'c' into the 'a' register.

## Four Byte Instructions
The first (lowest) byte of a four byte instruction  
must have the form 0b110xxxxx.

- 0b000000aa_aaabbbbb_mmxxxxxx_110_ddddd: ALU Op (0x1)  
    Perform the ALU operation given by 'x' on registers 'a' and 'b',  
    storing the result in 'd'.
    'm' indicates how the flags register should be handled:
    * 0b00 -> Force C to zero, discard I
    * 0b01 -> Force C to zero, store I in flags
    * 0b10 -> Read C from flags, dicard I
    * 0b11 -> Read C from flags, store I in flags

- 0b001cccaa_aaabbbbb_sszzcccc_110_ddddd: Load (0x3)  
    Compute an address as 'a' + ('b' << 's') + 'c'  
    and load 1, 2, 4 or 8 bytes into register 'd'.  
    Amount of bytes to load if given by 'z'.

- 0b010cccaa_aaabbbbb_sszzcccc_110_ddddd: Store (0x4)  
    Compute an address as 'a' + ('b' << 's') + 'c'  
    and store 1, 2, 4 or 8 bytes from register 'd'.  
    Amount of bytes to store if given by 'z'.

- 0b011cccaa_aaabbbbb_sscccccc_110_ddddd: Load Effective Address (0x5)  
    Compute an address as 'a' + ('b' << 's') + 'c'  
    and store it into the 'd' register.

- 0b10ccccaa_aaaccccc_mmxxxxxx_110_ddddd: ALU Op (0x2)  
    Perform the ALU operation given by 'x' with A = 'a' and B = 'c',  
    storing the result in 'd'.  
    'c' is a sign-extended constant value.  
    'm' indicates the flags mode as in (0x80).

- 0b11ccccaa_aaaccccc_mmxxxxxx_110_ddddd: ALU Op (0x6)  
    Perform the ALU operation given by 'x' with A = 'c' and B = 'a',  
    storing the result in 'd'.  
    'c' is a sign-extended constant value.  
    'm' indicates the flags mode as in (0x86).

## Eight Byte Instructions

- 0x00000000000000, 0b1110_0000: Long Nop (0x0)  
    Do nothing

- 0xZZZZZZZZ, 0bmmxxxxxx_dddddaaa_aaZZZZZZ_1110_0001: ALU Op (0x2)  
    Perform the ALU op given by 'x' with A = 'a', B = 'Z'  
    and O = 'd'.  
    'Z' is a sign-extended immediate value.  
    'm' indicates the flags mode as in (0x80).

- 0xZZZZZZZZ, 0bmmxxxxxx_dddddaaa_aaZZZZZZ_1110_0010: ALU Op (0x6)  
    Perform the ALU op given by 'x' with A = 'Z', B = 'a'  
    and O = 'd'.  
    'Z' is a sign-extended immediate value.  
    'm' indicates the flags mode as in (0x80).

- 0xYYYYYYYY, 0bYYYYsszz_dddddaaa_aabbbbbY_1110_0011: Load (0x3)  
    Calculate an address as 'a' + ('b' << 's') + 'Y'  
    and load 1, 2, 4 or 8 bytes into register 'd'.  
    Amount of bytes to read is indicated by 'z'.  
    'Y' is a sign-extended immediate value.
    
- 0xYYYYYYYY, 0bYYYYsszz_dddddaaa_aabbbbbY_1110_0100: Store (0x4)  
    Calculate an address as 'a' + ('b' << 's') + 'Y'  
    and store 1, 2, 4 or 8 bytes from register 'd'.  
    Amount of bytes to read is indicated by 'z'.  
    'Y' is a sign-extended immediate value.

- 0xYYYYYYYY, 0bYYYYssYY_dddddaaa_aabbbbbY_1110_0101: Lea (0x5)  
    Calculate an address as 'a' + ('b' << 's') + 'Y'  
    and store 1, 2, 4 or 8 bytes from register 'd'.  
    Amount of bytes to read is indicated by 'z'.  
    'Y' is a sign-extended immediate value.

- 0xYYYYYYYY, 0bYYYYssYY_dddddaaa_aabbbbbY_1110_0110: Call (0x7)
    Store the current instruction pointer (rip) into the 'd' register.
    Then, calculate an address as 'a' + ('b' << 's') + 'Y' and jump to it.
