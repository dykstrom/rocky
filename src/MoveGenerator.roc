module [generate_moves, with_moves]

import BishopMoveGenerator
import Board exposing [Board, initial_board]
import Color exposing [Color]
import KingMoveGenerator
import KnightMoveGenerator
import Move exposing [Move]
import PawnMoveGenerator
import RookMoveGenerator
import QueenMoveGenerator

## Generate all pseudo legal moves. This method does not care about checks.
generate_moves : Board, Color -> List Move
generate_moves = |board, side_to_move|
    my_pieces = if side_to_move == White then board.white else board.black
    their_pieces = if side_to_move == White then board.black else board.white

    bishop_moves = BishopMoveGenerator.generate_moves(board, my_pieces, their_pieces)
    king_moves = KingMoveGenerator.generate_moves(board, my_pieces, their_pieces, side_to_move)
    knight_moves = KnightMoveGenerator.generate_moves(board, my_pieces, their_pieces)
    pawn_moves = PawnMoveGenerator.generate_moves(board, their_pieces)
    rook_moves = RookMoveGenerator.generate_moves(board, my_pieces, their_pieces)
    queen_moves = QueenMoveGenerator.generate_moves(board, my_pieces, their_pieces)

    bishop_moves
    |> List.concat(king_moves)
    |> List.concat(knight_moves)
    |> List.concat(pawn_moves)
    |> List.concat(rook_moves)
    |> List.concat(queen_moves)

# Initial position
expect
    moves = generate_moves(initial_board, White) |> to_str
    Set.from_list(moves) == Set.from_list(["b1a3", "b1c3", "g1f3", "g1h3", "a2a4", "b2b4", "c2c4", "d2d4", "e2e4", "f2f4", "g2g4", "h2h4", "a2a3", "b2b3", "c2c3", "d2d3", "e2e3", "f2f3", "g2g3", "h2h3"])

## Generate all pseudo legal moves for both sides,
## and return the board decorated with those moves.
with_moves : Board -> Board
with_moves = |board|
    { board & white_moves: generate_moves(board, White), black_moves: generate_moves(board, Black) }

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

to_str : List Move -> List Str
to_str = |moves|
    List.map(moves, |move| Move.to_str(move))
