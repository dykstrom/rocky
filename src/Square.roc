module [
    SquareId,
    SquareIdx,
    from_str,
    fr_to_id,
    idx_to_id,
    to_str,
    a1,
    b1,
    c1,
    d1,
    e1,
    f1,
    g1,
    h1,
    a2,
    b2,
    c2,
    d2,
    e2,
    f2,
    g2,
    h2,
    a3,
    b3,
    c3,
    d3,
    e3,
    f3,
    g3,
    h3,
    a4,
    b4,
    c4,
    d4,
    e4,
    f4,
    g4,
    h4,
    a5,
    b5,
    c5,
    d5,
    e5,
    f5,
    g5,
    h5,
    a6,
    b6,
    c6,
    d6,
    e6,
    f6,
    g6,
    h6,
    a7,
    b7,
    c7,
    d7,
    e7,
    f7,
    g7,
    h7,
    a8,
    b8,
    c8,
    d8,
    e8,
    f8,
    g8,
    h8,
    a1_idx,
    b1_idx,
    c1_idx,
    d1_idx,
    e1_idx,
    f1_idx,
    g1_idx,
    h1_idx,
    a2_idx,
    b2_idx,
    c2_idx,
    d2_idx,
    e2_idx,
    f2_idx,
    g2_idx,
    h2_idx,
    a3_idx,
    b3_idx,
    c3_idx,
    d3_idx,
    e3_idx,
    f3_idx,
    g3_idx,
    h3_idx,
    a4_idx,
    b4_idx,
    c4_idx,
    d4_idx,
    e4_idx,
    f4_idx,
    g4_idx,
    h4_idx,
    a5_idx,
    b5_idx,
    c5_idx,
    d5_idx,
    e5_idx,
    f5_idx,
    g5_idx,
    h5_idx,
    a6_idx,
    b6_idx,
    c6_idx,
    d6_idx,
    e6_idx,
    f6_idx,
    g6_idx,
    h6_idx,
    a7_idx,
    b7_idx,
    c7_idx,
    d7_idx,
    e7_idx,
    f7_idx,
    g7_idx,
    h7_idx,
    a8_idx,
    b8_idx,
    c8_idx,
    d8_idx,
    e8_idx,
    f8_idx,
    g8_idx,
    h8_idx,
]

import Num exposing [shift_left_by]
import S

SquareId : U64
SquareIdx : U64

# Square ID constants
a1 = shift_left_by(1u64, 0)
b1 = shift_left_by(1u64, 1)
c1 = shift_left_by(1u64, 2)
d1 = shift_left_by(1u64, 3)
e1 = shift_left_by(1u64, 4)
f1 = shift_left_by(1u64, 5)
g1 = shift_left_by(1u64, 6)
h1 = shift_left_by(1u64, 7)

a2 = shift_left_by(1u64, 8)
b2 = shift_left_by(1u64, 9)
c2 = shift_left_by(1u64, 10)
d2 = shift_left_by(1u64, 11)
e2 = shift_left_by(1u64, 12)
f2 = shift_left_by(1u64, 13)
g2 = shift_left_by(1u64, 14)
h2 = shift_left_by(1u64, 15)

a3 = shift_left_by(1u64, 16)
b3 = shift_left_by(1u64, 17)
c3 = shift_left_by(1u64, 18)
d3 = shift_left_by(1u64, 19)
e3 = shift_left_by(1u64, 20)
f3 = shift_left_by(1u64, 21)
g3 = shift_left_by(1u64, 22)
h3 = shift_left_by(1u64, 23)

a4 = shift_left_by(1u64, 24)
b4 = shift_left_by(1u64, 25)
c4 = shift_left_by(1u64, 26)
d4 = shift_left_by(1u64, 27)
e4 = shift_left_by(1u64, 28)
f4 = shift_left_by(1u64, 29)
g4 = shift_left_by(1u64, 30)
h4 = shift_left_by(1u64, 31)

a5 = shift_left_by(1u64, 32)
b5 = shift_left_by(1u64, 33)
c5 = shift_left_by(1u64, 34)
d5 = shift_left_by(1u64, 35)
e5 = shift_left_by(1u64, 36)
f5 = shift_left_by(1u64, 37)
g5 = shift_left_by(1u64, 38)
h5 = shift_left_by(1u64, 39)

a6 = shift_left_by(1u64, 40)
b6 = shift_left_by(1u64, 41)
c6 = shift_left_by(1u64, 42)
d6 = shift_left_by(1u64, 43)
e6 = shift_left_by(1u64, 44)
f6 = shift_left_by(1u64, 45)
g6 = shift_left_by(1u64, 46)
h6 = shift_left_by(1u64, 47)

a7 = shift_left_by(1u64, 48)
b7 = shift_left_by(1u64, 49)
c7 = shift_left_by(1u64, 50)
d7 = shift_left_by(1u64, 51)
e7 = shift_left_by(1u64, 52)
f7 = shift_left_by(1u64, 53)
g7 = shift_left_by(1u64, 54)
h7 = shift_left_by(1u64, 55)

a8 = shift_left_by(1u64, 56)
b8 = shift_left_by(1u64, 57)
c8 = shift_left_by(1u64, 58)
d8 = shift_left_by(1u64, 59)
e8 = shift_left_by(1u64, 60)
f8 = shift_left_by(1u64, 61)
g8 = shift_left_by(1u64, 62)
h8 = shift_left_by(1u64, 63)

# Square index constants
a1_idx = 0
b1_idx = 1
c1_idx = 2
d1_idx = 3
e1_idx = 4
f1_idx = 5
g1_idx = 6
h1_idx = 7

a2_idx = 8
b2_idx = 9
c2_idx = 10
d2_idx = 11
e2_idx = 12
f2_idx = 13
g2_idx = 14
h2_idx = 15

a3_idx = 16
b3_idx = 17
c3_idx = 18
d3_idx = 19
e3_idx = 20
f3_idx = 21
g3_idx = 22
h3_idx = 23

a4_idx = 24
b4_idx = 25
c4_idx = 26
d4_idx = 27
e4_idx = 28
f4_idx = 29
g4_idx = 30
h4_idx = 31

a5_idx = 32
b5_idx = 33
c5_idx = 34
d5_idx = 35
e5_idx = 36
f5_idx = 37
g5_idx = 38
h5_idx = 39

a6_idx = 40
b6_idx = 41
c6_idx = 42
d6_idx = 43
e6_idx = 44
f6_idx = 45
g6_idx = 46
h6_idx = 47

a7_idx = 48
b7_idx = 49
c7_idx = 50
d7_idx = 51
e7_idx = 52
f7_idx = 53
g7_idx = 54
h7_idx = 55

a8_idx = 56
b8_idx = 57
c8_idx = 58
d8_idx = 59
e8_idx = 60
f8_idx = 61
g8_idx = 62
h8_idx = 63

squares = "a1b1c1d1e1f1g1h1a2b2c2d2e2f2g2h2a3b3c3d3e3f3g3h3a4b4c4d4e4f4g4h4a5b5c5d5e5f5g5h5a6b6c6d6e6f6g6h6a7b7c7d7e7f7g7h7a8b8c8d8e8f8g8h8"

from_str : Str -> Result SquareIdx [SyntaxError]
from_str = |str|
    S.index_of(squares, str)
    |> Result.map_ok(|index| index // 2)
    |> Result.map_err(|_| SyntaxError)

expect from_str("a1") == Ok(a1_idx)
expect from_str("h1") == Ok(h1_idx)
expect from_str("e2") == Ok(e2_idx)
expect from_str("e4") == Ok(e4_idx)
expect from_str("a8") == Ok(a8_idx)
expect from_str("h8") == Ok(h8_idx)

to_str : SquareIdx -> Str
to_str = |square_idx|
    S.substr(squares, { start: (square_idx * 2), len: 2 }) |> Result.with_default("??") # Cannot fail?

expect to_str(a1_idx) == "a1"
expect to_str(h1_idx) == "h1"
expect to_str(a8_idx) == "a8"
expect to_str(h8_idx) == "h8"

## Convert a file and rank pair to a square ID.
fr_to_id : U8, U8 -> SquareId
fr_to_id = |file, rank|
    shift_left_by(1, (rank * 8 + file))

expect fr_to_id(0, 0) == a1
expect fr_to_id(7, 0) == h1
expect fr_to_id(0, 7) == a8
expect fr_to_id(7, 7) == h8

idx_to_id : SquareIdx -> SquareId
idx_to_id = |idx|
    shift_left_by(1u64, Num.to_u8(idx))

expect idx_to_id(a8_idx) == a8
