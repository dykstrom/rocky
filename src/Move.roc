module [
    Move,
    create,
    create_castling,
    create_capture,
    create_capture_promotion,
    create_promotion,
    create_en_passant,
    get_from,
    get_to,
    get_moved,
    get_captured,
    get_promoted,
    is_castling,
    is_en_passant,
    to_str,
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

import Num exposing [bitwise_and, bitwise_or, shift_left_by, shift_right_zf_by]
import Piece exposing [PieceIdx]
import Square exposing [
    SquareIdx,
    a2_idx,
    a3_idx,
    a4_idx,
    a5_idx,
    a6_idx,
    a7_idx,
    a8_idx,
    b1_idx,
    b2_idx,
    b3_idx,
    b4_idx,
    b5_idx,
    b6_idx,
    b7_idx,
    b8_idx,
    c2_idx,
    c3_idx,
    c4_idx,
    c5_idx,
    c6_idx,
    c7_idx,
    d1_idx,
    d2_idx,
    d3_idx,
    d4_idx,
    d5_idx,
    d6_idx,
    d7_idx,
    d8_idx,
    e1_idx,
    e2_idx,
    e3_idx,
    e4_idx,
    e5_idx,
    e6_idx,
    e7_idx,
    f1_idx,
    f2_idx,
    f3_idx,
    f4_idx,
    f5_idx,
    f6_idx,
    f7_idx,
    f8_idx,
    g1_idx,
    g2_idx,
    g3_idx,
    g4_idx,
    g5_idx,
    g6_idx,
    g7_idx,
    g8_idx,
    h2_idx,
    h3_idx,
    h4_idx,
    h5_idx,
    h6_idx,
    h7_idx,
    h8_idx,
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

from_offset = 0x06u8
moved_offset = 0x0cu8
promoted_offset = 0x11u8
captured_offset = 0x14u8

piece_mask = 0x07u64
square_mask = 0x3fu64
castle_mask = shift_left_by(0x01u64, 0x0f)
en_passant_mask = shift_left_by(0x01u64, 0x10)

captured_pawn = shift_left_by(Piece.pawn, captured_offset)

moved_king = shift_left_by(Piece.king, moved_offset)
moved_pawn = shift_left_by(Piece.pawn, moved_offset)

# Test moves
a2a3 = create(a2_idx, a3_idx, Piece.pawn)
a2a4 = create(a2_idx, a4_idx, Piece.pawn)
a7a5 = create(a7_idx, a5_idx, Piece.pawn)
a7a6 = create(a7_idx, a6_idx, Piece.pawn)
a7a8q = create_promotion(a7_idx, a8_idx, Piece.queen)
b2b3 = create(b2_idx, b3_idx, Piece.pawn)
b2b4 = create(b2_idx, b4_idx, Piece.pawn)
b7b5 = create(b7_idx, b5_idx, Piece.pawn)
b7b6 = create(b7_idx, b6_idx, Piece.pawn)
c2c3 = create(c2_idx, c3_idx, Piece.pawn)
c2c4 = create(c2_idx, c4_idx, Piece.pawn)
c7c5 = create(c7_idx, c5_idx, Piece.pawn)
c7c6 = create(c7_idx, c6_idx, Piece.pawn)
d1h5 = create(d1_idx, h5_idx, Piece.queen)
d2d3 = create(d2_idx, d3_idx, Piece.pawn)
d2d4 = create(d2_idx, d4_idx, Piece.pawn)
d5d4 = create(d5_idx, d4_idx, Piece.pawn)
d5e4 = create_capture(d5_idx, e4_idx, Piece.pawn, Piece.pawn)
d5f6 = create(d5_idx, f6_idx, Piece.knight)
d7d5 = create(d7_idx, d5_idx, Piece.pawn)
d7d6 = create(d7_idx, d6_idx, Piece.pawn)
d8h4 = create(d8_idx, h4_idx, Piece.queen)
e1g1 = create_castling(e1_idx, g1_idx)
e2e3 = create(e2_idx, e3_idx, Piece.pawn)
e2e4 = create(e2_idx, e4_idx, Piece.pawn)
e4e5 = create(e4_idx, e5_idx, Piece.pawn)
e4d5 = create_capture(e4_idx, d5_idx, Piece.pawn, Piece.pawn)
e7e5 = create(e7_idx, e5_idx, Piece.pawn)
e7e6 = create(e7_idx, e6_idx, Piece.pawn)
f1c4 = create(f1_idx, c4_idx, Piece.bishop)
f2f3 = create(f2_idx, f3_idx, Piece.pawn)
f2f4 = create(f2_idx, f4_idx, Piece.pawn)
f3g1 = create(f3_idx, g1_idx, Piece.knight)
f6g8 = create(f6_idx, g8_idx, Piece.knight)
f7f5 = create(f7_idx, f5_idx, Piece.pawn)
f7f6 = create(f7_idx, f6_idx, Piece.pawn)
f8c5 = create(f8_idx, c5_idx, Piece.bishop)
g2g3 = create(g2_idx, g3_idx, Piece.pawn)
g2g4 = create(g2_idx, g4_idx, Piece.pawn)
g7g5 = create(g7_idx, g5_idx, Piece.pawn)
g7g6 = create(g7_idx, g6_idx, Piece.pawn)
g7h8n = create_capture_promotion(g7_idx, h8_idx, Piece.rook, Piece.knight)
h2h3 = create(h2_idx, h3_idx, Piece.pawn)
h2h4 = create(h2_idx, h4_idx, Piece.pawn)
h4h5 = create(h4_idx, h5_idx, Piece.pawn)
h5g6 = create_en_passant(h5_idx, g6_idx)
h7h5 = create(h7_idx, h5_idx, Piece.pawn)
h7h6 = create(h7_idx, h6_idx, Piece.pawn)

nb1a3 = create(b1_idx, a3_idx, Piece.knight)
nb1c3 = create(b1_idx, c3_idx, Piece.knight)
nb8a6 = create(b8_idx, a6_idx, Piece.knight)
nb8c6 = create(b8_idx, c6_idx, Piece.knight)
ng1f3 = create(g1_idx, f3_idx, Piece.knight)
ng1h3 = create(g1_idx, h3_idx, Piece.knight)
ng8f6 = create(g8_idx, f6_idx, Piece.knight)

to_str : Move -> Str
to_str = |move|
    from_idx = bitwise_and(shift_right_zf_by(move, from_offset), square_mask)
    to_idx = bitwise_and(move, square_mask)
    promoted_idx = bitwise_and(shift_right_zf_by(move, promoted_offset), piece_mask)
    Str.trim(Str.concat(Str.concat(Square.to_str(from_idx), Square.to_str(to_idx)), Piece.to_str(promoted_idx)))

expect to_str(e2e4) == "e2e4"
expect to_str(a7a8q) == "a7a8q"
expect to_str(g7h8n) == "g7h8n"

get_from : Move -> SquareIdx
get_from = |move|
    bitwise_and(shift_right_zf_by(move, from_offset), square_mask)

expect get_from(e2e4) == e2_idx
expect get_from(a7a8q) == a7_idx
expect get_from(g7h8n) == g7_idx

get_to : Move -> SquareIdx
get_to = |move|
    bitwise_and(move, square_mask)

expect get_to(e2e4) == e4_idx
expect get_to(a7a8q) == a8_idx
expect get_to(g7h8n) == h8_idx

get_moved : Move -> PieceIdx
get_moved = |move|
    bitwise_and(shift_right_zf_by(move, moved_offset), piece_mask)

expect get_moved(e2e4) == Piece.pawn
expect get_moved(ng1f3) == Piece.knight
expect get_moved(a7a8q) == Piece.pawn
expect get_moved(g7h8n) == Piece.pawn
expect get_moved(e1g1) == Piece.king

get_promoted : Move -> PieceIdx
get_promoted = |move|
    bitwise_and(shift_right_zf_by(move, promoted_offset), piece_mask)

expect get_promoted(e2e4) == Piece.none
expect get_promoted(a7a8q) == Piece.queen
expect get_promoted(g7h8n) == Piece.knight

get_captured : Move -> PieceIdx
get_captured = |move|
    bitwise_and(shift_right_zf_by(move, captured_offset), piece_mask)

expect get_captured(e2e4) == Piece.none
expect get_captured(e4d5) == Piece.pawn
expect get_captured(g7h8n) == Piece.rook

is_castling : Move -> Bool
is_castling = |move|
    bitwise_and(move, castle_mask) != 0

expect is_castling(e1g1)
expect is_castling(e4d5) == Bool.false
expect is_castling(g7h8n) == Bool.false

is_en_passant : Move -> Bool
is_en_passant = |move|
    bitwise_and(move, en_passant_mask) != 0

expect is_en_passant(h5g6)
expect is_en_passant(e1g1) == Bool.false
expect is_en_passant(e4d5) == Bool.false
expect is_en_passant(g7h8n) == Bool.false

# ----------------------------------------------------------------------------
# Creating moves
# ----------------------------------------------------------------------------

create : SquareIdx, SquareIdx, PieceIdx -> Move
create = |from_idx, to_idx, moved_idx|
    (shift_left_by(moved_idx, moved_offset))
    |> bitwise_or(shift_left_by(from_idx, from_offset))
    |> bitwise_or(to_idx)

create_promotion : SquareIdx, SquareIdx, PieceIdx -> Move
create_promotion = |from_idx, to_idx, promoted_idx|
    moved_pawn
    |> bitwise_or(shift_left_by(promoted_idx, promoted_offset))
    |> bitwise_or(shift_left_by(from_idx, from_offset))
    |> bitwise_or(to_idx)

create_capture : SquareIdx, SquareIdx, PieceIdx, PieceIdx -> Move
create_capture = |from_idx, to_idx, moved_idx, captured_idx|
    (shift_left_by(moved_idx, moved_offset))
    |> bitwise_or(shift_left_by(captured_idx, captured_offset))
    |> bitwise_or(shift_left_by(from_idx, from_offset))
    |> bitwise_or(to_idx)

create_capture_promotion : SquareIdx, SquareIdx, PieceIdx, PieceIdx -> Move
create_capture_promotion = |from_idx, to_idx, captured_idx, promoted_idx|
    moved_pawn
    |> bitwise_or(shift_left_by(captured_idx, captured_offset))
    |> bitwise_or(shift_left_by(promoted_idx, promoted_offset))
    |> bitwise_or(shift_left_by(from_idx, from_offset))
    |> bitwise_or(to_idx)

create_castling : SquareIdx, SquareIdx -> Move
create_castling = |from_idx, to_idx|
    moved_king
    |> bitwise_or(shift_left_by(from_idx, from_offset))
    |> bitwise_or(to_idx)
    |> bitwise_or(castle_mask)

create_en_passant : SquareIdx, SquareIdx -> Move
create_en_passant = |from_idx, to_idx|
    moved_pawn
    |> bitwise_or(captured_pawn)
    |> bitwise_or(shift_left_by(from_idx, from_offset))
    |> bitwise_or(to_idx)
    |> bitwise_or(en_passant_mask)
