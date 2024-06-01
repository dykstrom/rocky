module [
    SquareId,
    SquareIdx,
    fromStr,
    frToId,
    idxToId,
    toStr,
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
    a1Idx,
    b1Idx,
    c1Idx,
    d1Idx,
    e1Idx,
    f1Idx,
    g1Idx,
    h1Idx,
    a2Idx,
    b2Idx,
    c2Idx,
    d2Idx,
    e2Idx,
    f2Idx,
    g2Idx,
    h2Idx,
    a3Idx,
    b3Idx,
    c3Idx,
    d3Idx,
    e3Idx,
    f3Idx,
    g3Idx,
    h3Idx,
    a4Idx,
    b4Idx,
    c4Idx,
    d4Idx,
    e4Idx,
    f4Idx,
    g4Idx,
    h4Idx,
    a5Idx,
    b5Idx,
    c5Idx,
    d5Idx,
    e5Idx,
    f5Idx,
    g5Idx,
    h5Idx,
    a6Idx,
    b6Idx,
    c6Idx,
    d6Idx,
    e6Idx,
    f6Idx,
    g6Idx,
    h6Idx,
    a7Idx,
    b7Idx,
    c7Idx,
    d7Idx,
    e7Idx,
    f7Idx,
    g7Idx,
    h7Idx,
    a8Idx,
    b8Idx,
    c8Idx,
    d8Idx,
    e8Idx,
    f8Idx,
    g8Idx,
    h8Idx,
]

import Num exposing [shiftLeftBy]
import S

SquareId : U64
SquareIdx : U64

# Square ID constants
a1 = shiftLeftBy 1u64 0
b1 = shiftLeftBy 1u64 1
c1 = shiftLeftBy 1u64 2
d1 = shiftLeftBy 1u64 3
e1 = shiftLeftBy 1u64 4
f1 = shiftLeftBy 1u64 5
g1 = shiftLeftBy 1u64 6
h1 = shiftLeftBy 1u64 7

a2 = shiftLeftBy 1u64 8
b2 = shiftLeftBy 1u64 9
c2 = shiftLeftBy 1u64 10
d2 = shiftLeftBy 1u64 11
e2 = shiftLeftBy 1u64 12
f2 = shiftLeftBy 1u64 13
g2 = shiftLeftBy 1u64 14
h2 = shiftLeftBy 1u64 15

a3 = shiftLeftBy 1u64 16
b3 = shiftLeftBy 1u64 17
c3 = shiftLeftBy 1u64 18
d3 = shiftLeftBy 1u64 19
e3 = shiftLeftBy 1u64 20
f3 = shiftLeftBy 1u64 21
g3 = shiftLeftBy 1u64 22
h3 = shiftLeftBy 1u64 23

a4 = shiftLeftBy 1u64 24
b4 = shiftLeftBy 1u64 25
c4 = shiftLeftBy 1u64 26
d4 = shiftLeftBy 1u64 27
e4 = shiftLeftBy 1u64 28
f4 = shiftLeftBy 1u64 29
g4 = shiftLeftBy 1u64 30
h4 = shiftLeftBy 1u64 31

a5 = shiftLeftBy 1u64 32
b5 = shiftLeftBy 1u64 33
c5 = shiftLeftBy 1u64 34
d5 = shiftLeftBy 1u64 35
e5 = shiftLeftBy 1u64 36
f5 = shiftLeftBy 1u64 37
g5 = shiftLeftBy 1u64 38
h5 = shiftLeftBy 1u64 39

a6 = shiftLeftBy 1u64 40
b6 = shiftLeftBy 1u64 41
c6 = shiftLeftBy 1u64 42
d6 = shiftLeftBy 1u64 43
e6 = shiftLeftBy 1u64 44
f6 = shiftLeftBy 1u64 45
g6 = shiftLeftBy 1u64 46
h6 = shiftLeftBy 1u64 47

a7 = shiftLeftBy 1u64 48
b7 = shiftLeftBy 1u64 49
c7 = shiftLeftBy 1u64 50
d7 = shiftLeftBy 1u64 51
e7 = shiftLeftBy 1u64 52
f7 = shiftLeftBy 1u64 53
g7 = shiftLeftBy 1u64 54
h7 = shiftLeftBy 1u64 55

a8 = shiftLeftBy 1u64 56
b8 = shiftLeftBy 1u64 57
c8 = shiftLeftBy 1u64 58
d8 = shiftLeftBy 1u64 59
e8 = shiftLeftBy 1u64 60
f8 = shiftLeftBy 1u64 61
g8 = shiftLeftBy 1u64 62
h8 = shiftLeftBy 1u64 63

# Square index constants
a1Idx = 0
b1Idx = 1
c1Idx = 2
d1Idx = 3
e1Idx = 4
f1Idx = 5
g1Idx = 6
h1Idx = 7

a2Idx = 8
b2Idx = 9
c2Idx = 10
d2Idx = 11
e2Idx = 12
f2Idx = 13
g2Idx = 14
h2Idx = 15

a3Idx = 16
b3Idx = 17
c3Idx = 18
d3Idx = 19
e3Idx = 20
f3Idx = 21
g3Idx = 22
h3Idx = 23

a4Idx = 24
b4Idx = 25
c4Idx = 26
d4Idx = 27
e4Idx = 28
f4Idx = 29
g4Idx = 30
h4Idx = 31

a5Idx = 32
b5Idx = 33
c5Idx = 34
d5Idx = 35
e5Idx = 36
f5Idx = 37
g5Idx = 38
h5Idx = 39

a6Idx = 40
b6Idx = 41
c6Idx = 42
d6Idx = 43
e6Idx = 44
f6Idx = 45
g6Idx = 46
h6Idx = 47

a7Idx = 48
b7Idx = 49
c7Idx = 50
d7Idx = 51
e7Idx = 52
f7Idx = 53
g7Idx = 54
h7Idx = 55

a8Idx = 56
b8Idx = 57
c8Idx = 58
d8Idx = 59
e8Idx = 60
f8Idx = 61
g8Idx = 62
h8Idx = 63

squares = "a1b1c1d1e1f1g1h1a2b2c2d2e2f2g2h2a3b3c3d3e3f3g3h3a4b4c4d4e4f4g4h4a5b5c5d5e5f5g5h5a6b6c6d6e6f6g6h6a7b7c7d7e7f7g7h7a8b8c8d8e8f8g8h8"

fromStr : Str -> Result SquareIdx [SyntaxError]
fromStr = \str ->
    expect
        S.len str == 2

    S.indexOf squares str
    |> Result.map \index -> index // 2
    |> Result.mapErr \_ -> SyntaxError

expect fromStr "a1" == Ok a1Idx
expect fromStr "h1" == Ok h1Idx
expect fromStr "e2" == Ok e2Idx
expect fromStr "e4" == Ok e4Idx
expect fromStr "a8" == Ok a8Idx
expect fromStr "h8" == Ok h8Idx

toStr : SquareIdx -> Str
toStr = \squareIdx ->
    expect
        squareIdx >= 0 && squareIdx <= 63

    S.substr squares { start: (squareIdx * 2), len: 2 } |> Result.withDefault "??" # Cannot fail?

expect toStr a1Idx == "a1"
expect toStr h1Idx == "h1"
expect toStr a8Idx == "a8"
expect toStr h8Idx == "h8"

## Convert a file and rank pair to a square ID.
frToId : U8, U8 -> SquareId
frToId = \file, rank ->
    expect
        file >= 0 && file <= 7 && rank >= 0 && rank <= 7

    shiftLeftBy 1 (rank * 8 + file)

expect frToId 0 0 == a1
expect frToId 7 0 == h1
expect frToId 0 7 == a8
expect frToId 7 7 == h8

idxToId : SquareIdx -> SquareId
idxToId = \idx ->
    expect
        idx >= 0 && idx <= 63

    shiftLeftBy 1u64 (Num.toU8 idx)

expect idxToId a8Idx == a8
