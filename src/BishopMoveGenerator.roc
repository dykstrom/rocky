module [generate_moves]

import Board exposing [Bitboard, Board, initial_board]
import Move exposing [Move]
import Num exposing [bitwise_and]
import Piece
import SlideMoveGenerator exposing [ne, se, sw, nw]
import Util

generate_moves : Board, Bitboard, Bitboard -> List Move
generate_moves = |board, my_pieces, their_pieces|
    # Find from squares
    bishops = bitwise_and(my_pieces, board.bishop)
    bishop_idxs = Board.bb_to_idxs(bishops)
    # For each from square, generate all possible bishop moves
    List.walk(
        bishop_idxs,
        [],
        |list, idx|
            List.concat(list, SlideMoveGenerator.generate_moves_from_square(board, my_pieces, their_pieces, idx, Piece.bishop, [ne, se, sw, nw])),
    )

# Initial position
expect generate_moves(initial_board, initial_board.white, initial_board.black) == []
# After 1. e4 e5
expect
    board = Util.with_moves(initial_board, ["e2e4", "e7e5"], White)
    moves = generate_moves(board, board.white, board.black) |> to_str
    Set.from_list(moves) == Set.from_list(["f1e2", "f1d3", "f1c4", "f1b5", "f1a6"])

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

to_str : List Move -> List Str
to_str = |moves|
    List.map(moves, |move| Move.to_str(move))
