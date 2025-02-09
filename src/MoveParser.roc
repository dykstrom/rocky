module [is_move, parse]

import Board exposing [Board, initial_board]
import Checker
import Color exposing [Color]
import Move exposing [Move]
import MoveGenerator
import Num exposing [bitwise_xor]
import Piece exposing [PieceIdx]
import S
import Square exposing [SquareIdx, a2, a7, a8, b1_idx, c1_idx, c8_idx, e1_idx, e8_idx, f5_idx, g1_idx, g8_idx]
import Util

is_move : Str -> Bool
is_move = |str|
    if S.len(str) == 4 or S.len(str) == 5 then
        Result.is_ok(parse_from(str)) and Result.is_ok(parse_to(str))
    else
        Bool.false

parse : Board, Color, Str -> Result Move [SyntaxError, IllegalMove]
parse = |board, side_to_move, str|
    if S.len(str) == 4 or S.len(str) == 5 then
        from = parse_from(str)
        to = parse_to(str)
        promoted = parse_promoted(str)
        captured = Result.map_ok(to, |t| Board.piece_at(board, Square.idx_to_id(t)))
        captured_color = Result.try(to, |t| Board.color_at(board, Square.idx_to_id(t)))

        if Result.is_ok(from) and Result.is_ok(to) then
            from_idx = Result.with_default(from, 0)
            to_idx = Result.with_default(to, 0)
            moved_idx = Board.piece_at(board, Square.idx_to_id(from_idx))
            moved_color = Board.color_at(board, Square.idx_to_id(from_idx))
            if
                (from_idx == to_idx)
                or (moved_idx == Piece.none)
                or (moved_color == Err(SquareIsEmpty))
                or (moved_color != Ok(side_to_move))
                or (captured_color == Ok(side_to_move))
                or (captured == Ok(Piece.king))
                or (promoted == Ok(Piece.king))
                or (promoted == Ok(Piece.pawn))
                or (moved_idx == Piece.pawn and moved_color == Ok(White) and to_idx < from_idx)
                or (moved_idx == Piece.pawn and moved_color == Ok(Black) and to_idx > from_idx)
            then
                Err(IllegalMove)
            else
                create_and_test_move(board, side_to_move, from_idx, to_idx, moved_idx, captured, promoted)
        else
            Err(SyntaxError)
    else
        Err(SyntaxError)

# Ok: Pawn move
expect parse(initial_board, White, "e2e4") == Ok(Move.e2e4)
# Ok: Knight move
expect parse(initial_board, White, "g1f3") == Ok(Move.ng1f3)
# Ok: Promotion
expect
    b = { initial_board &
        # Move the White pawn from a2 to a7, and remove the Black pieces from a7 and a8
        white: bitwise_xor(bitwise_xor(initial_board.white, a2), a7),
        black: bitwise_xor(bitwise_xor(initial_board.black, a7), a8),
        pawn: bitwise_xor(initial_board.pawn, a2),
        rook: bitwise_xor(initial_board.rook, a8),
    }
    parse(b, White, "a7a8q") == Ok(Move.a7a8q)
# Err: Promote to king
expect
    b = { initial_board &
        # Move the White pawn from a2 to a7, and remove the Black pieces from a7 and a8
        white: bitwise_xor(bitwise_xor(initial_board.white, a2), a7),
        black: bitwise_xor(bitwise_xor(initial_board.black, a7), a8),
        pawn: bitwise_xor(initial_board.pawn, a2),
        rook: bitwise_xor(initial_board.rook, a8),
    }
    parse(b, White, "a7a8k") == Err(IllegalMove)
# Err: Invalid length
expect parse(initial_board, White, "a7a") == Err(SyntaxError)
# Err: Invalid contents
expect parse(initial_board, White, "k2k4") == Err(SyntaxError)
# Err: No piece on 'from' square
expect parse(initial_board, White, "e4e5") == Err(IllegalMove)
# Ok: Black move
expect
    board = Util.with_moves(initial_board, ["e2e4"], White)
    parse(board, Black, "d7d5") == Ok(Move.d7d5)
# Ok: White captures Black
expect
    board = Util.with_moves(initial_board, ["e2e4", "d7d5"], White)
    parse(board, White, "e4d5") == Ok(Move.e4d5)
# Err: White captures White
expect parse(initial_board, White, "a1b1") == Err(IllegalMove)
# Err: Castling with blocking pieces
expect parse(initial_board, White, "e1g1") == Err(IllegalMove)
# Ok: White castles
expect
    board = Util.with_moves(initial_board, ["e2e4", "e7e5", "g1f3", "g8f6", "f1c4", "f8c5"], White)
    parse(board, White, "e1g1") == Ok(Move.e1g1)
# Err: White is in check
expect
    board = Util.with_moves(initial_board, ["f2f3", "e7e5", "g2g4", "d8h4"], White)
    parse(board, White, "e2e4") == Err(IllegalMove)
# Err: From and to squares are the same
expect parse(initial_board, White, "a1a1") == Err(IllegalMove)
# Err: White pawn moves backwards
expect
    board = Util.with_moves(initial_board, ["e2e4", "e7e5"], White)
    parse(board, White, "e4e2") == Err(IllegalMove)
# Err: Black pawn moves backwards
expect
    board = Util.with_moves(initial_board, ["e2e4", "e7e5", "d2d4"], White)
    parse(board, Black, "e5e7") == Err(IllegalMove)

create_and_test_move :
    Board,
    Color,
    SquareIdx,
    SquareIdx,
    PieceIdx,
    Result PieceIdx _,
    Result PieceIdx _
    ->
    Result Move _
create_and_test_move = |board, side_to_move, from_idx, to_idx, moved_idx, captured, promoted|
    optional_move =
        if is_en_passant(board, to_idx, moved_idx) then
            Ok(Move.create_en_passant(from_idx, to_idx))
        else if is_castling(from_idx, to_idx, moved_idx) then
            castle(board, from_idx, to_idx)
        else if is_capture(captured) and is_promotion(promoted) then
            capture_and_promote(from_idx, to_idx, captured, promoted)
        else if is_capture(captured) then
            capture(from_idx, to_idx, moved_idx, captured)
        else if is_promotion(promoted) then
            promote(from_idx, to_idx, promoted)
        else
            # Normal move!
            Ok(Move.create(from_idx, to_idx, moved_idx))
    when optional_move is
        Ok(move) -> test_move(board, side_to_move, move)
        Err(error) -> Err(error)

test_move : Board, Color, Move -> Result Move [IllegalMove]
test_move = |board, side_to_move, move|
    board_after_move = Board.make_move(board, move, side_to_move)
    board_with_moves = MoveGenerator.with_moves(board_after_move)
    if Checker.is_check(board_with_moves, side_to_move) then
        Err(IllegalMove)
    else
        Ok(move)

is_promotion : Result PieceIdx _ -> Bool
is_promotion = |promoted|
    when promoted is
        Ok(piece_idx) -> piece_idx != Piece.none
        Err(_) -> Bool.false

is_capture : Result PieceIdx _ -> Bool
is_capture = |captured|
    when captured is
        Ok(piece_idx) -> piece_idx != Piece.none
        Err(_) -> Bool.false

expect is_capture(Ok(Piece.bishop))
expect is_capture(Ok(Piece.pawn))
expect is_capture(Ok(Piece.none)) == Bool.false
expect is_capture(Err(SyntaxError)) == Bool.false

capture = |from_idx, to_idx, moved_idx, captured|
    captured_idx = Result.with_default(captured, Piece.none)
    Ok(Move.create_capture(from_idx, to_idx, moved_idx, captured_idx))

capture_and_promote = |from_idx, to_idx, captured, promoted|
    promoted_idx = Result.with_default(promoted, 0)
    captured_idx = Result.with_default(captured, Piece.none)
    Ok(Move.create_capture_promotion(from_idx, to_idx, captured_idx, promoted_idx))

promote = |from_idx, to_idx, promoted|
    promoted_idx = Result.with_default(promoted, 0)
    Ok(Move.create_promotion(from_idx, to_idx, promoted_idx))

is_castling : SquareIdx, SquareIdx, PieceIdx -> Bool
is_castling = |from_idx, to_idx, moved_idx|
    (moved_idx == Piece.king)
    and
    ((from_idx == e1_idx and to_idx == c1_idx) or (from_idx == e1_idx and to_idx == g1_idx) or (from_idx == e8_idx and to_idx == c8_idx) or (from_idx == e8_idx and to_idx == g8_idx))

expect is_castling(e1_idx, c1_idx, Piece.king)
expect is_castling(e1_idx, g1_idx, Piece.king)
expect is_castling(e8_idx, c8_idx, Piece.king)
expect is_castling(e8_idx, g8_idx, Piece.king)
expect is_castling(f5_idx, g8_idx, Piece.king) == Bool.false
expect is_castling(e1_idx, b1_idx, Piece.king) == Bool.false
expect is_castling(e1_idx, c1_idx, Piece.rook) == Bool.false

is_en_passant : Board, SquareIdx, PieceIdx -> Bool
is_en_passant = |board, to_idx, moved_idx|
    (moved_idx == Piece.pawn)
    and (Board.is_en_passant_allowed(board))
    and (Board.en_passant_square(board) == to_idx)

castle : Board, SquareIdx, SquareIdx -> Result Move [IllegalMove]
castle = |board, from_idx, to_idx|
    # We already checked that this is a castling move, now we only check that it is legal
    if Board.is_castling_allowed(board, from_idx, to_idx) then
        Ok(Move.create_castling(from_idx, to_idx))
    else
        Err(IllegalMove)

parse_from : Str -> Result SquareIdx [BadUtf8, SyntaxError]
parse_from = |str|
    S.substr(str, { start: 0, len: 2 }) |> Result.try(Square.from_str)

parse_to : Str -> Result SquareIdx [BadUtf8, SyntaxError]
parse_to = |str|
    S.substr(str, { start: 2, len: 2 }) |> Result.try(Square.from_str)

parse_promoted : Str -> Result PieceIdx [BadUtf8, SyntaxError]
parse_promoted = |str|
    if S.len(str) == 5 then
        S.substr(str, { start: 4, len: 1 }) |> Result.try(Piece.from_str)
    else
        Err(SyntaxError)
