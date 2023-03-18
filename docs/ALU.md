# ALU Operations
The ALU has 3 data inputs (A, B, C)
and 2 outputs (O, I).  
It also has a 6-bit operation selector.
The operations are split into 8 families  
with 8 operations each.

## Unary Operations (0o0):
- 0o0: Bitwise Not  
    O = ~A
- 0o1: Negation (Two's Complement)  
    O = -A

## Binary Operations (0o1):
- 0o0: Bitwise And  
    O = A & B
- 0o1: Bitwise Or  
    O = A | B
- 0o2: Bitwise Xor  
    O = A ^ B
- 0o3: Shift Left  
    O = A << B
- 0o4: Shift Right Logical  
    O = A >> B
- 0o5: Shift Right Arithmetic  
    O = A >> B
- 0o6: Compose  
    O = (A << 32) | B

## Trinary Operations (0o2):
- 0o0: Add  
    O = A + B + C  
    I = carry
- 0o1: Sub  
    O = A - B - C  
    I = borrow
- 0o2: Mul (unsigned)  
    O = lower((A * B) + C)  
    I = higher((A * B) + C)
- 0o3: IMul (signed)  
    O = lower((A * B) + C)  
    I = higher((A * B) + C)
- 0o4: Select  
    O = C ? B : A

## Unary Test (0o3):
- 0o0: Test Zero  
    O = I = A == 0
- 0o1: Test Not Zero  
    O = I = A != 0
- 0o2: Test Negative  
    O = I = A < 0
- 0o3: Test Not Negative  
    O = I = A >= 0

## Relational Test (0o4):
- 0o0: Test Greater (signed)  
    O = I = A > B
- 0o1: Test Less (signed)  
    O = I = A < B
- 0o2: Test Above (unsigned)  
    O = I = A > B
- 0o3: Test Below (unsigned)  
    O = I = A < B
- 0o4: Test Not Greater (signed)  
    O = I = A <= B
- 0o5: Test Not Less (signed)  
    O = I = A >= B
- 0o6: Test Not Above (unsigned)  
    O = I = A <= B
- 0o7: Test Not Below (unsigned)
    O = I = A >= B

## Equality Test (0o5):
- 0o0: Test Equal  
    O = I = A == B
- 0o1: Test Not Equal  
    O = I = A != B

## Misc (0o6):
- 0o0: A Passthrough  
    O = A
    I = A
- 0o1: C Passthrough  
    O = C
    I = C
