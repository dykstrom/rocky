module [generateMoves]

import Board exposing [Bitboard, Board, initialBoard]
import Move exposing [Move]
import Num exposing [bitwiseAnd]
import Piece
import SlideMoveGenerator exposing [ne, se, sw, nw]
import Util

generateMoves : Board, Bitboard, Bitboard -> List Move
generateMoves = \board, myPieces, theirPieces ->
    # Find from squares
    bishops = bitwiseAnd myPieces board.bishop
    bishopIdxs = Board.bbToIdxs bishops
    # For each from square, generate all possible bishop moves
    List.walk bishopIdxs [] \list, idx ->
        List.concat list (SlideMoveGenerator.generateMovesFromSquare board myPieces theirPieces idx Piece.bishop [ne, se, sw, nw])

# Initial position
expect generateMoves initialBoard initialBoard.white initialBoard.black == []
# After 1. e4 e5
expect
    board = Util.withMoves initialBoard ["e2e4", "e7e5"] White
    moves = generateMoves board board.white board.black |> toStr
    Set.fromList moves == Set.fromList ["f1e2", "f1d3", "f1c4", "f1b5", "f1a6"]

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

toStr : List Move -> List Str
toStr = \moves ->
    List.map moves \move -> Move.toStr move
