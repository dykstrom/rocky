module [Bitboard, Board, empty_board, initial_board, make_move, color_at, piece_at, is_legal, is_castling_allowed, with_castling_rights, is_en_passant_allowed, en_passant_square, with_en_passant_square, half_move_clock, with_half_move_clock, equals_pieces, to_str, to_pretty_str, with_moves, bb_to_str, bb_to_idxs]

import Ansi
import Color exposing [Color]
import L
import Move exposing [Move, e1g1, e2e4, e7e5, f1c4, f3g1, f6g8, f8c5, g7g5, h2h4, h4h5, h5g6, ng1f3, ng8f6]
import Num exposing [bitwise_and, bitwise_not, bitwise_or, bitwise_xor, shift_left_by, shift_right_zf_by]
import Piece exposing [PieceIdx]
import S
import Square exposing [SquareId, SquareIdx, a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2, e4, g5, g6, a7, b7, c7, d7, e7, f7, g7, h7, a8, b8, c8, d8, e8, f8, g8, h8, a1_idx, b1_idx, b8_idx, c1_idx, c8_idx, d1_idx, d5_idx, d6_idx, d7_idx, e1_idx, e3_idx, e8_idx, f1_idx, g1_idx, g6_idx, g8_idx, h1_idx, a2_idx, b2_idx, c2_idx, d2_idx, e2_idx, f2_idx, g2_idx, h2_idx]

Bitboard : U64

Board : {
    white : Bitboard,
    black : Bitboard,
    bishop : Bitboard,
    king : Bitboard,
    knight : Bitboard,
    queen : Bitboard,
    pawn : Bitboard,
    rook : Bitboard,
    flags : U64,
    white_moves : List Move,
    black_moves : List Move,
}

square_mask = 0x3fu64

# The field flags is a bitset of flags that define the following data:
# - Bit 00-06 - half-move clock
# - Bit 07-08 - castling rights for white (1 = yes, 0 = no)
# - Bit 09-0a - castling rights for black (1 = yes, 0 = no)
# - Bit 0b-10 - en passant square
half_move_mask = 0x7fu64
wks_castling_mask = shift_left_by(1u64, 0x07u8)
wqs_castling_mask = shift_left_by(1u64, 0x08u8)
bks_castling_mask = shift_left_by(1u64, 0x09u8)
bqs_castling_mask = shift_left_by(1u64, 0x0au8)
en_passant_offset = 0x0bu8
en_passant_mask = shift_left_by(square_mask, en_passant_offset)

not_half_move_mask = bitwise_not(half_move_mask)
not_white_castling_mask = bitwise_not(bitwise_or(wks_castling_mask, wqs_castling_mask))
not_black_castling_mask = bitwise_not(bitwise_or(bks_castling_mask, bqs_castling_mask))
not_en_passant_mask = bitwise_not(en_passant_mask)

empty_board = {
    white: 0,
    black: 0,
    bishop: 0,
    king: 0,
    knight: 0,
    queen: 0,
    pawn: 0,
    rook: 0,
    flags: 0,
    white_moves: [],
    black_moves: [],
}

initial_board = {
    white: L.or_list([a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2]),
    black: L.or_list([a7, b7, c7, d7, e7, f7, g7, h7, a8, b8, c8, d8, e8, f8, g8, h8]),
    bishop: L.or_list([c1, f1, c8, f8]),
    king: L.or_list([e1, e8]),
    knight: L.or_list([b1, g1, b8, g8]),
    queen: L.or_list([d1, d8]),
    pawn: L.or_list([a2, b2, c2, d2, e2, f2, g2, h2, a7, b7, c7, d7, e7, f7, g7, h7]),
    rook: L.or_list([a1, h1, a8, h8]),
    flags: L.or_list([wks_castling_mask, wqs_castling_mask, bks_castling_mask, bqs_castling_mask]),
    white_moves: [],
    black_moves: [],
}

expect bitwise_and(initial_board.rook, a1) != 0
expect bitwise_and(initial_board.rook, h1) != 0
expect bitwise_and(initial_board.rook, a8) != 0
expect bitwise_and(initial_board.rook, h8) != 0

# Squares that must be empty when castling
b1c1d1 = L.or_list([b1, c1, d1])
f1g1 = L.or_list([f1, g1])
b8c8d8 = L.or_list([b8, c8, d8])
f8g8 = L.or_list([f8, g8])

color_at : Board, SquareId -> Result Color [SquareIsEmpty]
color_at = |board, square|
    if bitwise_and(board.white, square) != 0 then
        Ok(White)
    else if bitwise_and(board.black, square) != 0 then
        Ok(Black)
    else
        Err(SquareIsEmpty)

expect color_at(initial_board, e1) == Ok(White)
expect color_at(initial_board, e8) == Ok(Black)
expect color_at(initial_board, e4) == Err(SquareIsEmpty)

piece_at : Board, SquareId -> PieceIdx
piece_at = |board, square|
    if bitwise_and(board.bishop, square) != 0 then
        Piece.bishop
    else if bitwise_and(board.king, square) != 0 then
        Piece.king
    else if bitwise_and(board.knight, square) != 0 then
        Piece.knight
    else if bitwise_and(board.pawn, square) != 0 then
        Piece.pawn
    else if bitwise_and(board.queen, square) != 0 then
        Piece.queen
    else if bitwise_and(board.rook, square) != 0 then
        Piece.rook
    else
        Piece.none

expect piece_at(initial_board, e1) == Piece.king
expect piece_at(initial_board, b8) == Piece.knight
expect piece_at(initial_board, h8) == Piece.rook
expect piece_at(initial_board, e4) == Piece.none

is_legal : Board -> Bool
is_legal = |board|
    (List.len(bb_to_idxs(bitwise_and(board.white, board.king))) == 1)
    and
    (List.len(bb_to_idxs(bitwise_and(board.black, board.king))) == 1)

expect is_legal(initial_board) == Bool.true
expect is_legal(empty_board) == Bool.false

is_castling_allowed : Board, SquareIdx, SquareIdx -> Bool
is_castling_allowed = |board, from_idx, to_idx|
    if from_idx == e1_idx and to_idx == c1_idx then
        ica(board, e1, a1, b1c1d1, wqs_castling_mask)
    else if from_idx == e1_idx and to_idx == g1_idx then
        ica(board, e1, h1, f1g1, wks_castling_mask)
    else if from_idx == e8_idx and to_idx == c8_idx then
        ica(board, e8, a8, b8c8d8, bqs_castling_mask)
    else if from_idx == e8_idx and to_idx == g8_idx then
        ica(board, e8, h8, f8g8, bks_castling_mask)
    else
        Bool.false

expect is_castling_allowed(initial_board, e1_idx, c1_idx) == Bool.false
expect is_castling_allowed(initial_board, e1_idx, g1_idx) == Bool.false
expect is_castling_allowed(initial_board, e8_idx, c8_idx) == Bool.false
expect is_castling_allowed(initial_board, e8_idx, g8_idx) == Bool.false

ica : Board, SquareId, SquareId, Bitboard, U64 -> Bool
ica = |board, king_square, rook_square, blocking_squares, castling_mask|
    (piece_at(board, rook_square) == Piece.rook)
    and (color_at(board, king_square) == color_at(board, rook_square))
    and ((bitwise_and(bitwise_or(board.white, board.black), blocking_squares)) == 0)
    and (bitwise_and(board.flags, castling_mask) != 0)

is_en_passant_allowed : Board -> Bool
is_en_passant_allowed = |board|
    # This works because the en passant square is always on the 3rd or 6th rank.
    # It can never be a1, which would return false here.
    bitwise_and(board.flags, en_passant_mask) != 0

expect is_en_passant_allowed(initial_board) == Bool.false
expect
    board = { initial_board &
        flags: bitwise_or(initial_board.flags, shift_left_by(g6_idx, en_passant_offset)),
    }
    is_en_passant_allowed(board)

en_passant_square : Board -> SquareIdx
en_passant_square = |board|
    shift_right_zf_by(bitwise_and(board.flags, en_passant_mask), en_passant_offset)

expect
    board = { initial_board & flags: bitwise_or(initial_board.flags, shift_left_by(g6_idx, en_passant_offset)) }
    en_passant_square(board) == g6_idx

half_move_clock : Board -> U64
half_move_clock = |board|
    bitwise_and(board.flags, half_move_mask)

expect half_move_clock(initial_board) == 0
expect
    board = { initial_board & flags: initial_board.flags + 53 }
    half_move_clock(board) == 53

## Return true if the two boards are equal if only the pieces are considered.
equals_pieces : Board, Board -> Bool
equals_pieces = |board1, board2|
    (board1.white == board2.white)
    and (board1.black == board2.black)
    and (board1.bishop == board2.bishop)
    and (board1.king == board2.king)
    and (board1.knight == board2.knight)
    and (board1.pawn == board2.pawn)
    and (board1.queen == board2.queen)
    and (board1.rook == board2.rook)

expect
    board = with_moves(initial_board, [ng1f3, ng8f6, f3g1, f6g8], White)
    (board != initial_board)
    and
    (equals_pieces(board, initial_board) == Bool.true)

to_str : Board -> Str
to_str = |board|
    ranks = List.range({ start: At(7), end: At(0) })
    files = List.range({ start: At(0), end: At(7) })

    List.map(
        ranks,
        |r|
            List.map(
                files,
                |f|
                    square = Square.fr_to_id(f, r)
                    color = color_at(board, square)
                    piece = piece_at(board, square)
                    piece_str = if piece == Piece.none then "." else Piece.to_str(piece)
                    if color == Ok(White) then
                        Result.with_default(S.to_upper(piece_str), "?")
                    else
                        piece_str,
            )
            |> Str.join_with("")
            |> |line| Str.concat(Num.to_str((r + 1)), " | ") |> Str.concat(line) |> Str.concat(" |"),
    )
    |> Str.join_with("\n")
    |> |s| Str.concat("   ---------- \n", s)
    |> Str.concat("\n   ---------- \n    abcdefgh ")

to_pretty_str : Board -> Str
to_pretty_str = |board|
    ranks = List.range({ start: At(7), end: At(0) })
    files = List.range({ start: At(0), end: At(7) })

    List.map(
        ranks,
        |r|
            List.map(
                files,
                |f|
                    square = Square.fr_to_id(f, r)
                    color = color_at(board, square)
                    piece = piece_at(board, square)
                    character =
                        when color is
                            Ok(White) -> Piece.to_pretty_str(piece, White)
                            Ok(Black) -> Piece.to_pretty_str(piece, Black)
                            _ -> " "
                    Str.concat(background_for(f, r), character) |> Str.concat(Ansi.default),
            )
            |> Str.join_with("")
            |> |line| Str.concat(Num.to_str((r + 1)), " │ ") |> Str.concat(line) |> Str.concat(" │"),
    )
    |> Str.join_with("\n")
    |> |s| Str.concat("  ┌──────────┐\n", s)
    |> Str.concat("\n  └──────────┘\n    abcdefgh ")

background_for : U8, U8 -> Str
background_for = |file, rank|
    if file % 2 == rank % 2 then Ansi.dark else Ansi.light

make_move : Board, Move, Color -> Board
make_move = |board, move, color|
    from_id = Square.idx_to_id(Move.get_from(move))
    to_id = Square.idx_to_id(Move.get_to(move))
    moved = Move.get_moved(move)
    captured = Move.get_captured(move)
    promoted = Move.get_promoted(move)

    after_move =
        if Move.is_castling(move) then
            castle(board, from_id, to_id, color)
        else if Move.is_en_passant(move) then
            move_from(board, from_id, moved, color)
            |> move_to(to_id, moved, color)
            |> capture_en_passant(to_id, color)
        else
            move_from(board, from_id, moved, color)
            |> move_to(to_id, moved, color)
            |> |b| if captured != Piece.none then capture_piece(b, to_id, captured, color) else b
            |> |b| if promoted != Piece.none then promote_piece(b, to_id, promoted) else b
    after_move
    |> update_castling_flags(from_id)
    |> update_en_passant_square(Move.get_from(move), Move.get_to(move), moved)
    |> update_half_move_clock(moved, (captured != Piece.none))

# Ok: White move
expect
    new_board = make_move(initial_board, e2e4, White)
    new_board.white
    == bitwise_or(bitwise_xor(initial_board.white, e2), e4)
    and new_board.black
    == initial_board.black
    and new_board.bishop
    == initial_board.bishop
    and new_board.knight
    == initial_board.knight
    and new_board.king
    == initial_board.king
    and new_board.pawn
    == bitwise_or(bitwise_xor(initial_board.pawn, e2), e4)
    and new_board.queen
    == initial_board.queen
    and new_board.rook
    == initial_board.rook
    and is_en_passant_allowed(new_board)
    == Bool.true
    and en_passant_square(new_board)
    == e3_idx

# Ok: White castling
expect
    board_where_white_can_castle = with_moves(initial_board, [e2e4, e7e5, ng1f3, ng8f6, f1c4, f8c5], White)
    new_board = make_move(board_where_white_can_castle, e1g1, White)

    piece_at(new_board, e1)
    == Piece.none
    and piece_at(new_board, f1)
    == Piece.rook
    and piece_at(new_board, g1)
    == Piece.king
    and piece_at(new_board, h1)
    == Piece.none
    and color_at(new_board, e1)
    == Err(SquareIsEmpty)
    and color_at(new_board, f1)
    == Ok(White)
    and color_at(new_board, g1)
    == Ok(White)
    and color_at(new_board, h1)
    == Err(SquareIsEmpty)
    and bitwise_and(new_board.flags, wks_castling_mask)
    == 0
    and bitwise_and(new_board.flags, wqs_castling_mask)
    == 0

# Ok: White captures 'en passant' on g6
expect
    board = with_moves(initial_board, [h2h4, e7e5, h4h5, g7g5], White)
    new_board = make_move(board, h5g6, White)

    piece_at(new_board, g5)
    == Piece.none
    and piece_at(new_board, g6)
    == Piece.pawn
    and color_at(new_board, g5)
    == Err(SquareIsEmpty)
    and color_at(new_board, g6)
    == Ok(White)
    and is_en_passant_allowed(new_board)
    == Bool.false
    and en_passant_square(new_board)
    == 0

update_castling_flags : Board, SquareId -> Board
update_castling_flags = |board, from_id|
    if from_id == e1 then
        { board & flags: bitwise_and(board.flags, not_white_castling_mask) }
    else if from_id == a1 then
        { board & flags: bitwise_and(board.flags, bitwise_not(wqs_castling_mask)) }
    else if from_id == h1 then
        { board & flags: bitwise_and(board.flags, bitwise_not(wks_castling_mask)) }
    else if from_id == e8 then
        { board & flags: bitwise_and(board.flags, not_black_castling_mask) }
    else if from_id == a8 then
        { board & flags: bitwise_and(board.flags, bitwise_not(bqs_castling_mask)) }
    else if from_id == h8 then
        { board & flags: bitwise_and(board.flags, bitwise_not(bks_castling_mask)) }
    else
        board

with_castling_rights : Board, Color, PieceIdx -> Board
with_castling_rights = |board, color, piece|
    when (color, piece) is
        (White, 2) -> { board & flags: bitwise_or(board.flags, wks_castling_mask) }
        (White, 5) -> { board & flags: bitwise_or(board.flags, wqs_castling_mask) }
        (Black, 2) -> { board & flags: bitwise_or(board.flags, bks_castling_mask) }
        (Black, 5) -> { board & flags: bitwise_or(board.flags, bqs_castling_mask) }
        _ -> crash("Should not happen: unknown piece in withCastlingRights: ${Num.to_str(piece)}")

update_en_passant_square : Board, SquareIdx, SquareIdx, PieceIdx -> Board
update_en_passant_square = |board, from_idx, to_idx, moved|
    bitwise_and(board.flags, not_en_passant_mask)
    |> |flags|
        if moved == Piece.pawn then
            if to_idx == from_idx + 16 then
                bitwise_or(flags, shift_left_by((from_idx + 8), en_passant_offset))
            else if from_idx == to_idx + 16 then
                bitwise_or(flags, shift_left_by((to_idx + 8), en_passant_offset))
            else
                flags
        else
            flags
    |> |flags| { board & flags }

expect
    board = update_en_passant_square(initial_board, d7_idx, d5_idx, Piece.pawn)
    en_passant_square(board) == d6_idx and is_en_passant_allowed(board)
expect
    board = update_en_passant_square(initial_board, d6_idx, d5_idx, Piece.pawn)
    en_passant_square(board) == 0 and is_en_passant_allowed(board) == Bool.false
expect
    board = update_en_passant_square(initial_board, d7_idx, d5_idx, Piece.rook)
    en_passant_square(board) == 0 and is_en_passant_allowed(board) == Bool.false

with_en_passant_square : Board, SquareIdx -> Board
with_en_passant_square = |board, square|
    { board & flags: bitwise_and(board.flags, not_en_passant_mask) |> bitwise_or(shift_left_by(square, en_passant_offset)) }

expect
    board = with_en_passant_square(initial_board, d5_idx)
    (is_en_passant_allowed(board) == Bool.true)
    and (en_passant_square(board) == d5_idx)

update_half_move_clock : Board, PieceIdx, Bool -> Board
update_half_move_clock = |board, moved, is_capture|
    if is_capture or moved == Piece.pawn then
        { board & flags: bitwise_and(board.flags, not_half_move_mask) }
    else
        { board & flags: board.flags + 1 }

with_half_move_clock : Board, U64 -> Board
with_half_move_clock = |board, clock|
    { board & flags: bitwise_and(board.flags, not_half_move_mask) |> bitwise_or(clock) }

expect
    board = with_half_move_clock(initial_board, 17)
    half_move_clock(board) == 17

move_from : Board, SquareId, PieceIdx, Color -> Board
move_from = |board, square, moved, color|
    (new_white, new_black) =
        if color == White then
            (bitwise_xor(board.white, square), board.black)
        else
            (board.white, bitwise_xor(board.black, square))

    (new_bishop, new_king, new_knight, new_pawn, new_queen, new_rook) =
        when moved is
            1 -> (bitwise_xor(board.bishop, square), board.king, board.knight, board.pawn, board.queen, board.rook)
            2 -> (board.bishop, bitwise_xor(board.king, square), board.knight, board.pawn, board.queen, board.rook)
            3 -> (board.bishop, board.king, bitwise_xor(board.knight, square), board.pawn, board.queen, board.rook)
            4 -> (board.bishop, board.king, board.knight, bitwise_xor(board.pawn, square), board.queen, board.rook)
            5 -> (board.bishop, board.king, board.knight, board.pawn, bitwise_xor(board.queen, square), board.rook)
            6 -> (board.bishop, board.king, board.knight, board.pawn, board.queen, bitwise_xor(board.rook, square))
            _ -> crash("Should not happen: unknown piece in moveFrom: ${Num.to_str(moved)}")

    { board &
        white: new_white,
        black: new_black,
        bishop: new_bishop,
        king: new_king,
        knight: new_knight,
        pawn: new_pawn,
        queen: new_queen,
        rook: new_rook,
    }

move_to : Board, SquareId, PieceIdx, Color -> Board
move_to = |board, square, moved, color|
    (new_white, new_black) =
        if color == White then
            (bitwise_or(board.white, square), board.black)
        else
            (board.white, bitwise_or(board.black, square))

    (new_bishop, new_king, new_knight, new_pawn, new_queen, new_rook) =
        when moved is
            1 -> (bitwise_xor(board.bishop, square), board.king, board.knight, board.pawn, board.queen, board.rook)
            2 -> (board.bishop, bitwise_xor(board.king, square), board.knight, board.pawn, board.queen, board.rook)
            3 -> (board.bishop, board.king, bitwise_xor(board.knight, square), board.pawn, board.queen, board.rook)
            4 -> (board.bishop, board.king, board.knight, bitwise_xor(board.pawn, square), board.queen, board.rook)
            5 -> (board.bishop, board.king, board.knight, board.pawn, bitwise_xor(board.queen, square), board.rook)
            6 -> (board.bishop, board.king, board.knight, board.pawn, board.queen, bitwise_xor(board.rook, square))
            _ -> crash("Should not happen: unknown piece in moveTo: ${Num.to_str(moved)}")

    { board &
        white: new_white,
        black: new_black,
        bishop: new_bishop,
        king: new_king,
        knight: new_knight,
        pawn: new_pawn,
        queen: new_queen,
        rook: new_rook,
    }

promote_piece : Board, SquareId, PieceIdx -> Board
promote_piece = |board, square, promoted|
    { board &
        bishop: if promoted == Piece.bishop then bitwise_or(board.bishop, square) else board.bishop,
        knight: if promoted == Piece.knight then bitwise_or(board.knight, square) else board.knight,
        queen: if promoted == Piece.queen then bitwise_or(board.queen, square) else board.queen,
        rook: if promoted == Piece.rook then bitwise_or(board.rook, square) else board.rook,
        pawn: bitwise_xor(board.pawn, square),
    }

## Return the board with the captured piece removed.
capture_piece : Board, SquareId, PieceIdx, Color -> Board
capture_piece = |board, square, captured, color|
    { board &
        bishop: if captured == Piece.bishop then bitwise_xor(board.bishop, square) else board.bishop,
        king: if captured == Piece.king then bitwise_xor(board.king, square) else board.king,
        knight: if captured == Piece.knight then bitwise_xor(board.knight, square) else board.knight,
        pawn: if captured == Piece.pawn then bitwise_xor(board.pawn, square) else board.pawn,
        queen: if captured == Piece.queen then bitwise_xor(board.queen, square) else board.queen,
        rook: if captured == Piece.rook then bitwise_xor(board.rook, square) else board.rook,
        white: if color == White then board.white else bitwise_xor(board.white, square),
        black: if color == White then bitwise_xor(board.black, square) else board.black,
    }

## Return the board after castling with the king moving from kf to kt.
castle : Board, SquareId, SquareId, Color -> Board
castle = |board, kf, kt, color|
    (rf, rt) =
        if kt == c1 then
            (a1, d1)
        else if kt == g1 then
            (h1, f1)
        else if kt == c8 then
            (a8, d8)
        else if kt == g8 then
            (h8, f8)
        else
            crash("Should not happen: invalid from square: ${Square.to_str(kt)}")

    { board &
        white: if color == White then
            bitwise_xor(bitwise_xor(bitwise_xor(bitwise_xor(board.white, kf), kt), rf), rt)
        else
            board.white,
        black: if color == Black then
            bitwise_xor(bitwise_xor(bitwise_xor(bitwise_xor(board.black, kf), kt), rf), rt)
        else
            board.black,
        king: bitwise_xor(bitwise_xor(board.king, kf), kt),
        rook: bitwise_xor(bitwise_xor(board.rook, rf), rt),
    }

## Return the board with the piece captured 'en passant' removed.
capture_en_passant : Board, SquareId, Color -> Board
capture_en_passant = |board, to_id, side_to_move|
    captured_square_id =
        if side_to_move == White then
            shift_right_zf_by(to_id, 8)
        else
            shift_left_by(to_id, 8)
    { board &
        pawn: bitwise_xor(board.pawn, captured_square_id),
        white: if side_to_move == White then board.white else bitwise_xor(board.white, captured_square_id),
        black: if side_to_move == White then bitwise_xor(board.black, captured_square_id) else board.black,
    }

## Setup the given board by making the given moves.
with_moves : Board, List Move, Color -> Board
with_moves = |board, moves, side_to_move|
    List.walk(
        moves,
        { b: board, c: side_to_move },
        |state, move|
            new_board = make_move(state.b, move, state.c)
            new_color = Color.flip_color(state.c)
            { b: new_board, c: new_color },
    )
    |> |state| state.b

expect with_moves(initial_board, [ng1f3, ng8f6, f3g1, f6g8], White) == { initial_board & flags: initial_board.flags + 4 }

## Convert the given bitboard to a string of 0s and 1s.
bb_to_str : Bitboard -> Str
bb_to_str = |bitboard|
    ranks = List.range({ start: At(7), end: At(0) })
    files = List.range({ start: At(0), end: At(7) })

    List.map(
        ranks,
        |r|
            List.map(
                files,
                |f|
                    square = Square.fr_to_id(f, r)
                    if bitwise_and(bitboard, square) != 0 then "1" else "0",
            )
            |> Str.join_with(""),
    )
    |> Str.join_with("\n")
    |> |s| Str.concat(Str.concat("\n", s), "\n")

expect bb_to_str(initial_board.white) == "\n00000000\n00000000\n00000000\n00000000\n00000000\n00000000\n11111111\n11111111\n"

## Return a list of square indices: one index for each square that is occupied in the given bitboard.
bb_to_idxs : Bitboard -> List SquareIdx
bb_to_idxs = |bitboard|
    iter = |b, list|
        if b == 0 then
            list
        else
            idx = Num.count_trailing_zero_bits(b)
            next = bitwise_and(b, bitwise_not(shift_left_by(1u64, idx)))
            iter(next, List.append(list, Num.to_u64(idx)))
    iter(bitboard, List.with_capacity(64))

expect bb_to_idxs(0) == []
expect bb_to_idxs(initial_board.knight) == [b1_idx, g1_idx, b8_idx, g8_idx]
expect bb_to_idxs(initial_board.king) == [e1_idx, e8_idx]
expect bb_to_idxs(initial_board.white) == [a1_idx, b1_idx, c1_idx, d1_idx, e1_idx, f1_idx, g1_idx, h1_idx, a2_idx, b2_idx, c2_idx, d2_idx, e2_idx, f2_idx, g2_idx, h2_idx]
