module [isCheck]

import Board exposing [Board, initialBoard]
import Color exposing [Color]
import Move
import MoveGenerator
import Piece
import Util

# Return true if the side _not_ to move is in check.
# This is done by generating all pseudo legal moves
# and checking if any move captures the king.
isCheck : Board, Color -> Bool
isCheck = \board, sideToMove ->
    MoveGenerator.generateMoves board sideToMove
    |> List.any \move ->
        Move.getCaptured move == Piece.king

# Initial position, white
expect isCheck initialBoard White == Bool.false
# Initial position, black
expect isCheck initialBoard Black == Bool.false
# After 1. Nc3 a6 2. Nd5 b6 3. Nf6+, black is checked
expect
    board = Util.withMoves initialBoard ["b1c3", "a7a6", "c3d5", "b7b6", "d5f6"] White
    isCheck board White == Bool.true
