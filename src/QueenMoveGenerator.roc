module [generateMoves]

import Board exposing [Bitboard, Board, initialBoard]
import Move exposing [Move]
import Num exposing [bitwiseAnd]
import Piece
import SlideMoveGenerator exposing [n, e, s, w, ne, se, sw, nw]
import Util

generateMoves : Board, Bitboard, Bitboard -> List Move
generateMoves = \board, myPieces, theirPieces ->
    # Find from squares
    queens = bitwiseAnd myPieces board.queen
    queenIdxs = Board.bbToIdxs queens
    # For each from square, generate all possible queen moves
    List.walk queenIdxs [] \list, idx ->
        List.concat list (SlideMoveGenerator.generateMovesFromSquare board myPieces theirPieces idx Piece.queen [n, e, s, w, ne, se, sw, nw])

# Initial position
expect generateMoves initialBoard initialBoard.white initialBoard.black == []
# After 1. e4 h5 2. d4 d5
expect
    board = Util.withMoves initialBoard ["e2e4", "h7h5", "d2d4", "d7d5"] White
    moves = generateMoves board board.white board.black |> toStr
    Set.fromList moves == Set.fromList ["d1d2", "d1d3", "d1e2", "d1f3", "d1g4", "d1h5"]

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

toStr : List Move -> List Str
toStr = \moves ->
    List.map moves \move -> Move.toStr move
