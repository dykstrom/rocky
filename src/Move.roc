module [
    Move,
    create,
    createCastling,
    createCapture,
    createCapturePromotion,
    createPromotion,
    createEnPassant,
    getFrom,
    getTo,
    getMoved,
    getCaptured,
    getPromoted,
    isCastling,
    isEnPassant,
    toStr,
    a2a3,
    a2a4,
    a7a5,
    a7a6,
    a7a8q,
    b2b3,
    b2b4,
    b7b5,
    b7b6,
    c2c3,
    c2c4,
    c7c5,
    c7c6,
    d1h5,
    d2d3,
    d2d4,
    d5d4,
    d5e4,
    d5f6,
    d7d5,
    d7d6,
    d8h4,
    e1g1,
    e2e3,
    e2e4,
    e4d5,
    e4e5,
    e7e5,
    e7e6,
    f1c4,
    f2f3,
    f2f4,
    f3g1,
    f6g8,
    f7f5,
    f7f6,
    f8c5,
    g2g3,
    g2g4,
    g7g5,
    g7g6,
    h2h3,
    h2h4,
    h4h5,
    h5g6,
    h7h5,
    h7h6,
    nb1a3,
    nb1c3,
    nb8a6,
    nb8c6,
    ng1f3,
    ng1h3,
    ng8f6,
]

import Num exposing [bitwiseAnd, bitwiseOr, shiftLeftBy, shiftRightZfBy]
import Piece exposing [PieceIdx]
import Square exposing [
    SquareIdx,
    a2Idx,
    a3Idx,
    a4Idx,
    a5Idx,
    a6Idx,
    a7Idx,
    a8Idx,
    b1Idx,
    b2Idx,
    b3Idx,
    b4Idx,
    b5Idx,
    b6Idx,
    b7Idx,
    b8Idx,
    c2Idx,
    c3Idx,
    c4Idx,
    c5Idx,
    c6Idx,
    c7Idx,
    d1Idx,
    d2Idx,
    d3Idx,
    d4Idx,
    d5Idx,
    d6Idx,
    d7Idx,
    d8Idx,
    e1Idx,
    e2Idx,
    e3Idx,
    e4Idx,
    e5Idx,
    e6Idx,
    e7Idx,
    f1Idx,
    f2Idx,
    f3Idx,
    f4Idx,
    f5Idx,
    f6Idx,
    f7Idx,
    f8Idx,
    g1Idx,
    g2Idx,
    g3Idx,
    g4Idx,
    g5Idx,
    g6Idx,
    g7Idx,
    g8Idx,
    h2Idx,
    h3Idx,
    h4Idx,
    h5Idx,
    h6Idx,
    h7Idx,
    h8Idx,
]

Move : U64

# The individual bits in a move define the following data:
# - Bit 00-05 - to square
# - Bit 06-0b - from square
# - Bit 0c-0e - moved piece
# - Bit 0f    - castle flag
# - Bit 10    - en passant flag
# - Bit 11-13 - promoted piece
# - Bit 14-16 - captured piece

fromOffset = 0x06u8
movedOffset = 0x0cu8
promotedOffset = 0x11u8
capturedOffset = 0x14u8

pieceMask = 0x07u64
squareMask = 0x3fu64
castleMask = shiftLeftBy 0x01u64 0x0f
enPassantMask = shiftLeftBy 0x01u64 0x10

capturedPawn = shiftLeftBy Piece.pawn capturedOffset

movedKing = shiftLeftBy Piece.king movedOffset
movedPawn = shiftLeftBy Piece.pawn movedOffset

# Test moves
a2a3 = create a2Idx a3Idx Piece.pawn
a2a4 = create a2Idx a4Idx Piece.pawn
a7a5 = create a7Idx a5Idx Piece.pawn
a7a6 = create a7Idx a6Idx Piece.pawn
a7a8q = createPromotion a7Idx a8Idx Piece.queen
b2b3 = create b2Idx b3Idx Piece.pawn
b2b4 = create b2Idx b4Idx Piece.pawn
b7b5 = create b7Idx b5Idx Piece.pawn
b7b6 = create b7Idx b6Idx Piece.pawn
c2c3 = create c2Idx c3Idx Piece.pawn
c2c4 = create c2Idx c4Idx Piece.pawn
c7c5 = create c7Idx c5Idx Piece.pawn
c7c6 = create c7Idx c6Idx Piece.pawn
d1h5 = create d1Idx h5Idx Piece.queen
d2d3 = create d2Idx d3Idx Piece.pawn
d2d4 = create d2Idx d4Idx Piece.pawn
d5d4 = create d5Idx d4Idx Piece.pawn
d5e4 = createCapture d5Idx e4Idx Piece.pawn Piece.pawn
d5f6 = create d5Idx f6Idx Piece.knight
d7d5 = create d7Idx d5Idx Piece.pawn
d7d6 = create d7Idx d6Idx Piece.pawn
d8h4 = create d8Idx h4Idx Piece.queen
e1g1 = createCastling e1Idx g1Idx
e2e3 = create e2Idx e3Idx Piece.pawn
e2e4 = create e2Idx e4Idx Piece.pawn
e4e5 = create e4Idx e5Idx Piece.pawn
e4d5 = createCapture e4Idx d5Idx Piece.pawn Piece.pawn
e7e5 = create e7Idx e5Idx Piece.pawn
e7e6 = create e7Idx e6Idx Piece.pawn
f1c4 = create f1Idx c4Idx Piece.bishop
f2f3 = create f2Idx f3Idx Piece.pawn
f2f4 = create f2Idx f4Idx Piece.pawn
f3g1 = create f3Idx g1Idx Piece.knight
f6g8 = create f6Idx g8Idx Piece.knight
f7f5 = create f7Idx f5Idx Piece.pawn
f7f6 = create f7Idx f6Idx Piece.pawn
f8c5 = create f8Idx c5Idx Piece.bishop
g2g3 = create g2Idx g3Idx Piece.pawn
g2g4 = create g2Idx g4Idx Piece.pawn
g7g5 = create g7Idx g5Idx Piece.pawn
g7g6 = create g7Idx g6Idx Piece.pawn
g7h8n = createCapturePromotion g7Idx h8Idx Piece.rook Piece.knight
h2h3 = create h2Idx h3Idx Piece.pawn
h2h4 = create h2Idx h4Idx Piece.pawn
h4h5 = create h4Idx h5Idx Piece.pawn
h5g6 = createEnPassant h5Idx g6Idx
h7h5 = create h7Idx h5Idx Piece.pawn
h7h6 = create h7Idx h6Idx Piece.pawn

nb1a3 = create b1Idx a3Idx Piece.knight
nb1c3 = create b1Idx c3Idx Piece.knight
nb8a6 = create b8Idx a6Idx Piece.knight
nb8c6 = create b8Idx c6Idx Piece.knight
ng1f3 = create g1Idx f3Idx Piece.knight
ng1h3 = create g1Idx h3Idx Piece.knight
ng8f6 = create g8Idx f6Idx Piece.knight

toStr : Move -> Str
toStr = \move ->
    fromIdx = bitwiseAnd (shiftRightZfBy move fromOffset) squareMask
    toIdx = bitwiseAnd move squareMask
    promotedIdx = bitwiseAnd (shiftRightZfBy move promotedOffset) pieceMask
    Str.trim (Str.concat (Str.concat (Square.toStr fromIdx) (Square.toStr toIdx)) (Piece.toStr promotedIdx))

expect toStr e2e4 == "e2e4"
expect toStr a7a8q == "a7a8q"
expect toStr g7h8n == "g7h8n"

getFrom : Move -> SquareIdx
getFrom = \move ->
    bitwiseAnd (shiftRightZfBy move fromOffset) squareMask

expect getFrom e2e4 == e2Idx
expect getFrom a7a8q == a7Idx
expect getFrom g7h8n == g7Idx

getTo : Move -> SquareIdx
getTo = \move ->
    bitwiseAnd move squareMask

expect getTo e2e4 == e4Idx
expect getTo a7a8q == a8Idx
expect getTo g7h8n == h8Idx

getMoved : Move -> PieceIdx
getMoved = \move ->
    bitwiseAnd (shiftRightZfBy move movedOffset) pieceMask

expect getMoved e2e4 == Piece.pawn
expect getMoved ng1f3 == Piece.knight
expect getMoved a7a8q == Piece.pawn
expect getMoved g7h8n == Piece.pawn
expect getMoved e1g1 == Piece.king

getPromoted : Move -> PieceIdx
getPromoted = \move ->
    bitwiseAnd (shiftRightZfBy move promotedOffset) pieceMask

expect getPromoted e2e4 == Piece.none
expect getPromoted a7a8q == Piece.queen
expect getPromoted g7h8n == Piece.knight

getCaptured : Move -> PieceIdx
getCaptured = \move ->
    bitwiseAnd (shiftRightZfBy move capturedOffset) pieceMask

expect getCaptured e2e4 == Piece.none
expect getCaptured e4d5 == Piece.pawn
expect getCaptured g7h8n == Piece.rook

isCastling : Move -> Bool
isCastling = \move ->
    bitwiseAnd move castleMask != 0

expect isCastling e1g1
expect isCastling e4d5 == Bool.false
expect isCastling g7h8n == Bool.false

isEnPassant : Move -> Bool
isEnPassant = \move ->
    bitwiseAnd move enPassantMask != 0

expect isEnPassant h5g6
expect isEnPassant e1g1 == Bool.false
expect isEnPassant e4d5 == Bool.false
expect isEnPassant g7h8n == Bool.false

# ----------------------------------------------------------------------------
# Creating moves
# ----------------------------------------------------------------------------

create : SquareIdx, SquareIdx, PieceIdx -> Move
create = \fromIdx, toIdx, movedIdx ->
    (shiftLeftBy movedIdx movedOffset)
    |> bitwiseOr (shiftLeftBy fromIdx fromOffset)
    |> bitwiseOr toIdx

createPromotion : SquareIdx, SquareIdx, PieceIdx -> Move
createPromotion = \fromIdx, toIdx, promotedIdx ->
    movedPawn
    |> bitwiseOr (shiftLeftBy promotedIdx promotedOffset)
    |> bitwiseOr (shiftLeftBy fromIdx fromOffset)
    |> bitwiseOr toIdx

createCapture : SquareIdx, SquareIdx, PieceIdx, PieceIdx -> Move
createCapture = \fromIdx, toIdx, movedIdx, capturedIdx ->
    (shiftLeftBy movedIdx movedOffset)
    |> bitwiseOr (shiftLeftBy capturedIdx capturedOffset)
    |> bitwiseOr (shiftLeftBy fromIdx fromOffset)
    |> bitwiseOr toIdx

createCapturePromotion : SquareIdx, SquareIdx, PieceIdx, PieceIdx -> Move
createCapturePromotion = \fromIdx, toIdx, capturedIdx, promotedIdx ->
    movedPawn
    |> bitwiseOr (shiftLeftBy capturedIdx capturedOffset)
    |> bitwiseOr (shiftLeftBy promotedIdx promotedOffset)
    |> bitwiseOr (shiftLeftBy fromIdx fromOffset)
    |> bitwiseOr toIdx

createCastling : SquareIdx, SquareIdx -> Move
createCastling = \fromIdx, toIdx ->
    movedKing
    |> bitwiseOr (shiftLeftBy fromIdx fromOffset)
    |> bitwiseOr toIdx
    |> bitwiseOr castleMask

createEnPassant : SquareIdx, SquareIdx -> Move
createEnPassant = \fromIdx, toIdx ->
    movedPawn
    |> bitwiseOr capturedPawn
    |> bitwiseOr (shiftLeftBy fromIdx fromOffset)
    |> bitwiseOr toIdx
    |> bitwiseOr enPassantMask
