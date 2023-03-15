# ALU Operations
The ALU has 3 data inputs (A, B, C)
and 2 outputs (O, I).  
It also has a 6-bit operation selector.
The operations are split into 8 families  
with 8 operations each.

## Unary Operations (0x0):
- 0x0: Bitwise Not  
    O = ~A
- 0x1: Negation (Two's Complement)  
    O = -A

## Binary Operations (0x1):
- 0x0: Bitwise And  
    O = A & B
- 0x1: Bitwise Or  
    O = A | B
- 0x2: Bitwise Xor  
    O = A ^ B
- 0x3: Shift Left  
    O = A << B
- 0x4: Shift Right Logical  
    O = A >> B
- 0x5: Shift Right Arithmetic  
    O = A >> B
- 0x6: Compose  
    O = (A << 32) | B

## Trinary Operations (0x2):
- 0x0: Add  
    O = A + B + C  
    I = carry
- 0x1: Sub  
    O = A - B - C  
    I = borrow
- 0x2: Mul (unsigned)  
    O = lower((A * B) + C)  
    I = higher((A * B) + C)
- 0x3: IMul (signed)  
    O = lower((A * B) + C)  
    I = higher((A * B) + C)
- 0x4: Select  
    O = C ? B : A

## Unary Test (0x3):
- 0x0: Test Zero  
    O = I = A == 0
- 0x1: Test Not Zero  
    O = I = A != 0
- 0x2: Test Negative  
    O = I = A < 0
- 0x3: Test Not Negative  
    O = I = A >= 0

## Relational Test (0x4):
- 0x0: Test Greater (signed)  
    O = I = A > B
- 0x1: Test Less (signed)  
    O = I = A < B
- 0x2: Test Above (unsigned)  
    O = I = A > B
- 0x3: Test Below (unsigned)  
    O = I = A < B
- 0x4: Test Not Greater (signed)  
    O = I = A <= B
- 0x5: Test Not Less (signed)  
    O = I = A >= B
- 0x6: Test Not Above (unsigned)  
    O = I = A <= B
- 0x7: Test Not Below (unsigned)
    O = I = A >= B

## Equality Test (0x5):
- 0x0: Test Equal  
    O = I = A == B
- 0x1: Test Not Equal  
    O = I = A != B

## Misc (0x6):
- 0x0: A Passthrough  
    O = A
    I = A
- 0x1: C Passthrough  
    O = C
    I = C
