module [generate_moves]

import Board exposing [Bitboard, Board, initial_board]
import Move exposing [Move]
import Num exposing [bitwise_and]
import Piece
import SlideMoveGenerator exposing [n, e, s, w, ne, se, sw, nw]
import Util

generate_moves : Board, Bitboard, Bitboard -> List Move
generate_moves = |board, my_pieces, their_pieces|
    # Find from squares
    queens = bitwise_and(my_pieces, board.queen)
    queen_idxs = Board.bb_to_idxs(queens)
    # For each from square, generate all possible queen moves
    List.walk(
        queen_idxs,
        [],
        |list, idx|
            List.concat(list, SlideMoveGenerator.generate_moves_from_square(board, my_pieces, their_pieces, idx, Piece.queen, [n, e, s, w, ne, se, sw, nw])),
    )

# Initial position
expect generate_moves(initial_board, initial_board.white, initial_board.black) == []
# After 1. e4 h5 2. d4 d5
expect
    board = Util.with_moves(initial_board, ["e2e4", "h7h5", "d2d4", "d7d5"], White)
    moves = generate_moves(board, board.white, board.black) |> to_str
    Set.from_list(moves) == Set.from_list(["d1d2", "d1d3", "d1e2", "d1f3", "d1g4", "d1h5"])

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

to_str : List Move -> List Str
to_str = |moves|
    List.map(moves, |move| Move.to_str(move))
