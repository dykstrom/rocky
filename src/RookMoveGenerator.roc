module [generateMoves]

import Board exposing [Bitboard, Board, initialBoard]
import Move exposing [Move]
import Num exposing [bitwiseAnd]
import Piece
import SlideMoveGenerator exposing [n, e, s, w]
import Util

generateMoves : Board, Bitboard, Bitboard -> List Move
generateMoves = \board, myPieces, theirPieces ->
    # Find from squares
    rooks = bitwiseAnd myPieces board.rook
    rookIdxs = Board.bbToIdxs rooks
    # For each from square, generate all possible rook moves
    List.walk rookIdxs [] \list, idx ->
        List.concat list (SlideMoveGenerator.generateMovesFromSquare board myPieces theirPieces idx Piece.rook [n, e, s, w])

# Initial position
expect generateMoves initialBoard initialBoard.white initialBoard.black == []
# After 1. a4 e5 2. Ra3 Nc6
expect
    board = Util.withMoves initialBoard ["a2a4", "e7e5", "a1a3", "b8c6"] White
    moves = generateMoves board board.white board.black |> toStr
    Set.fromList moves == Set.fromList ["a3a2", "a3a1", "a3b3", "a3c3", "a3d3", "a3e3", "a3f3", "a3g3", "a3h3"]

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

toStr : List Move -> List Str
toStr = \moves ->
    List.map moves \move -> Move.toStr move
