module [debug, with_moves, with_history]

import Board exposing [Board, initial_board]
import Color exposing [Color]
import Game exposing [Game]
import Move exposing [Move]
import Piece exposing [PieceIdx]
import S
import Square exposing [SquareIdx, c1_idx, c8_idx, e1_idx, e8_idx, g1_idx, g8_idx]

## Setup the given board by making the given moves.
with_moves : Board, List Str, Color -> Board
with_moves = |board, moves, side_to_move|
    List.walk(
        moves,
        { b: board, c: side_to_move },
        |state, str|
            move = parse_move(state.b, str)
            new_board = Board.make_move(state.b, move, state.c)
            new_color = Color.flip_color(state.c)
            { b: new_board, c: new_color },
    )
    |> |state| state.b

expect with_moves(initial_board, ["g1f3", "g8f6", "f3g1", "f6g8"], White) == { initial_board & flags: initial_board.flags + 4 }

## Setup the given board by making the given moves. Return the board and the board history.
with_history : Board, List Str, Color -> { board : Board, history : List Board }
with_history = |board, moves, side_to_move|
    List.walk(
        moves,
        { b: board, h: [], c: side_to_move },
        |state, str|
            move = parse_move(state.b, str)
            new_board = Board.make_move(state.b, move, state.c)
            new_color = Color.flip_color(state.c)
            { b: new_board, h: List.append(state.h, new_board), c: new_color },
    )
    |> |state| { board: state.b, history: state.h }

## Parse and return the given move string. This is a simplified parse method to be ued in tests only.
## It can only handle legal moves. We cannot use the MoveParser to parse moves, because that would
## cause an illegal import cycle.
parse_move : Board, Str -> Move
parse_move = |board, str|
    from_idx =
        str
        |> S.substr({ start: 0, len: 2 })
        |> Result.try(Square.from_str)
        |> Result.with_default(0)
    to_idx =
        str
        |> S.substr({ start: 2, len: 2 })
        |> Result.try(Square.from_str)
        |> Result.with_default(0)
    promoted_idx =
        str
        |> |s| if S.len(s) == 5 then S.substr(s, { start: 4, len: 1 }) else Err(NotFound)
        |> Result.try(Piece.from_str)
        |> Result.with_default(0)
    moved_idx = Board.piece_at(board, Square.idx_to_id(from_idx))
    captured_idx = Board.piece_at(board, Square.idx_to_id(to_idx))

    if is_en_passant(board, to_idx, moved_idx) then
        Move.create_castling(from_idx, to_idx)
    else if is_castling(from_idx, to_idx, moved_idx) then
        Move.create_castling(from_idx, to_idx)
    else if captured_idx != 0 and promoted_idx != 0 then
        Move.create_capture_promotion(from_idx, to_idx, captured_idx, promoted_idx)
    else if captured_idx != 0 then
        Move.create_capture(from_idx, to_idx, moved_idx, captured_idx)
    else if promoted_idx != 0 then
        Move.create_promotion(from_idx, to_idx, promoted_idx)
    else
        Move.create(from_idx, to_idx, moved_idx)

expect parse_move(initial_board, "e2e4") == Move.e2e4
expect parse_move(initial_board, "e1g1") == Move.e1g1

is_en_passant : Board, SquareIdx, PieceIdx -> Bool
is_en_passant = |board, to_idx, moved_idx|
    (moved_idx == Piece.pawn)
    and (Board.is_en_passant_allowed(board))
    and (Board.en_passant_square(board) == to_idx)

is_castling : SquareIdx, SquareIdx, PieceIdx -> Bool
is_castling = |from_idx, to_idx, moved_idx|
    (moved_idx == Piece.king)
    and
    ((from_idx == e1_idx and to_idx == c1_idx) or (from_idx == e1_idx and to_idx == g1_idx) or (from_idx == e8_idx and to_idx == c8_idx) or (from_idx == e8_idx and to_idx == g8_idx))

## If debug is enabled, return the message prefixed by '#'. Otherwise, return an empty string.
debug : Game, Str -> Str
debug = |game, msg|
    if game.debug == On then
        Str.concat("# ", msg) |> Str.concat("\n")
    else
        ""
