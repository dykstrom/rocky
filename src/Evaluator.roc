module [evaluate, illegal_check_value, checkmate_value, draw_value]

import Board exposing [Bitboard, Board, initial_board]
import Checker
import Color exposing [Color]
import L
import MoveGenerator
import Num exposing [bitwise_and, bitwise_xor]
import Square exposing [a1, f5, h8]
import Util

bishop_value = 3_000
knight_value = 3_000
queen_value = 9_000
pawn_value = 1_000
rook_value = 5_000

check_value = 100
mobility_value = 10
illegal_check_value = -100_000
checkmate_value = -illegal_check_value
draw_value = 0

## Evaluate the given board and calculate the score for the given color.
## The score will be positive if the given color is in the lead.
evaluate : Board, Color -> I64
evaluate = |board, color|
    mobility_score = evaluate_mobility(board)
    check_score = evaluate_check(board, color)
    material_score = evaluate_material(board)
    (if color == White then mobility_score else -mobility_score)
    + check_score
    + (if color == White then material_score else -material_score)

# Initial position
expect
    board_with_moves = MoveGenerator.with_moves(initial_board)
    evaluate(board_with_moves, White) == 0
expect
    board_with_moves = MoveGenerator.with_moves(initial_board)
    evaluate(board_with_moves, Black) == 0
# Equal position
expect
    board = Util.with_moves(initial_board, ["e2e4", "e7e5"], White)
    board_with_moves = MoveGenerator.with_moves(board)
    evaluate(board_with_moves, Black) == 0
# White is a rook down
expect
    board = { initial_board &
        white: bitwise_xor(initial_board.white, a1),
        rook: bitwise_xor(initial_board.rook, a1),
    }
    board_with_moves = MoveGenerator.with_moves(board)
    evaluate(board_with_moves, White) == (-rook_value)
# White is checked but not checkmated (he can escape to e2)
expect
    board = Util.with_moves(initial_board, ["f2f3", "e7e5", "e2e4", "d8h4"], White)
    board_with_moves = MoveGenerator.with_moves(board)
    evaluate(board_with_moves, Black) > 0
# White is checkmated, but we only know that White is checked
expect
    board = Util.with_moves(initial_board, ["f2f3", "e7e5", "g2g4", "d8h4"], White)
    board_with_moves = MoveGenerator.with_moves(board)
    evaluate(board_with_moves, Black) > 0

## Return a small positive score if opponent is in check.
evaluate_check : Board, Color -> I64
evaluate_check = |board, color|
    if Checker.is_check(board, Color.flip_color(color)) then check_value else 0

## Evaluate mobility. The score will be positive if White is in the lead.
evaluate_mobility : Board -> I64
evaluate_mobility = |board|
    (Num.to_i64(List.len(board.white_moves)) - Num.to_i64(List.len(board.black_moves))) * mobility_value

## Evaluate material on the board. The score will be positive if White is in the lead.
evaluate_material : Board -> I64
evaluate_material = |board|
    (pop_count(bitwise_and(board.white, board.bishop)) * bishop_value)
    + (pop_count(bitwise_and(board.white, board.knight)) * knight_value)
    + (pop_count(bitwise_and(board.white, board.pawn)) * pawn_value)
    + (pop_count(bitwise_and(board.white, board.queen)) * queen_value)
    + (pop_count(bitwise_and(board.white, board.rook)) * rook_value)
    - (pop_count(bitwise_and(board.black, board.bishop)) * bishop_value)
    - (pop_count(bitwise_and(board.black, board.knight)) * knight_value)
    - (pop_count(bitwise_and(board.black, board.pawn)) * pawn_value)
    - (pop_count(bitwise_and(board.black, board.queen)) * queen_value)
    - (pop_count(bitwise_and(board.black, board.rook)) * rook_value)

pop_count : Bitboard -> I64
pop_count = |bitboard|
    Num.to_i64(Num.count_one_bits(bitboard))

expect pop_count(0) == 0
expect pop_count(1) == 1
expect pop_count(2) == 1
expect pop_count(3) == 2
expect pop_count(4) == 1
expect pop_count(7) == 3
expect pop_count(h8) == 1
expect pop_count(L.or_list([a1, f5, h8])) == 3
expect pop_count(initial_board.white) == 16
expect pop_count(initial_board.bishop) == 4
