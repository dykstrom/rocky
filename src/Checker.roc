module [is_check, is_draw_by50_move_rule, is_draw_by_threefold_repetition]

import Board exposing [Board, initial_board]
import Color exposing [Color]
import Move
import MoveGenerator
import Piece
import Util

## Return true if the given color is in check.
## This is done by generating all pseudo legal moves
## and testing if any move captures the king.
is_check : Board, Color -> Bool
is_check = |board, color|
    expect
        board.white_moves != []

    expect
        board.black_moves != []

    moves = if color == White then board.black_moves else board.white_moves
    List.any(
        moves,
        |move|
            Move.get_captured(move) == Piece.king,
    )

# Initial position, white
expect
    board_with_moves = MoveGenerator.with_moves(initial_board)
    is_check(board_with_moves, White) == Bool.false
# Initial position, black
expect
    board_with_moves = MoveGenerator.with_moves(initial_board)
    is_check(board_with_moves, Black) == Bool.false
# After 1. Nc3 a6 2. Nd5 b6 3. Nf6+, black is checked
expect
    board = Util.with_moves(initial_board, ["b1c3", "a7a6", "c3d5", "b7b6", "d5f6"], White)
    board_with_moves = MoveGenerator.with_moves(board)
    is_check(board_with_moves, Black) == Bool.true

is_draw_by50_move_rule : Board -> Bool
is_draw_by50_move_rule = |board|
    Board.half_move_clock(board) >= 100

expect is_draw_by50_move_rule(initial_board) == Bool.false
expect
    board = { initial_board & flags: initial_board.flags + 100 }
    is_draw_by50_move_rule(board) == Bool.true

is_draw_by_threefold_repetition : Board, List Board -> Bool
is_draw_by_threefold_repetition = |board, board_history|
    (List.count_if(board_history, |b| Board.equals_pieces(b, board))) >= 3

expect is_draw_by_threefold_repetition(initial_board, []) == Bool.false
expect is_draw_by_threefold_repetition(initial_board, [initial_board, initial_board, initial_board]) == Bool.true
