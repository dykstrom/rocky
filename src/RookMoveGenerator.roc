module [generate_moves]

import Board exposing [Bitboard, Board, initial_board]
import Move exposing [Move]
import Num exposing [bitwise_and]
import Piece
import SlideMoveGenerator exposing [n, e, s, w]
import Util

generate_moves : Board, Bitboard, Bitboard -> List Move
generate_moves = |board, my_pieces, their_pieces|
    # Find from squares
    rooks = bitwise_and(my_pieces, board.rook)
    rook_idxs = Board.bb_to_idxs(rooks)
    # For each from square, generate all possible rook moves
    List.walk(
        rook_idxs,
        [],
        |list, idx|
            List.concat(list, SlideMoveGenerator.generate_moves_from_square(board, my_pieces, their_pieces, idx, Piece.rook, [n, e, s, w])),
    )

# Initial position
expect generate_moves(initial_board, initial_board.white, initial_board.black) == []
# After 1. a4 e5 2. Ra3 Nc6
expect
    board = Util.with_moves(initial_board, ["a2a4", "e7e5", "a1a3", "b8c6"], White)
    moves = generate_moves(board, board.white, board.black) |> to_str
    Set.from_list(moves) == Set.from_list(["a3a2", "a3a1", "a3b3", "a3c3", "a3d3", "a3e3", "a3f3", "a3g3", "a3h3"])

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

to_str : List Move -> List Str
to_str = |moves|
    List.map(moves, |move| Move.to_str(move))
