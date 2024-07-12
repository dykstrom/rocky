module [isCheck, isDrawBy50MoveRule, isDrawByThreefoldRepetition]

import Board exposing [Board, initialBoard]
import Color exposing [Color]
import Move
import MoveGenerator
import Piece
import Util

## Return true if the given color is in check.
## This is done by generating all pseudo legal moves
## and testing if any move captures the king.
isCheck : Board, Color -> Bool
isCheck = \board, color ->
    MoveGenerator.generateMoves board (Color.flipColor color)
    |> List.any \move ->
        Move.getCaptured move == Piece.king

# Initial position, white
expect isCheck initialBoard White == Bool.false
# Initial position, black
expect isCheck initialBoard Black == Bool.false
# After 1. Nc3 a6 2. Nd5 b6 3. Nf6+, black is checked
expect
    board = Util.withMoves initialBoard ["b1c3", "a7a6", "c3d5", "b7b6", "d5f6"] White
    isCheck board Black == Bool.true

isDrawBy50MoveRule : Board -> Bool
isDrawBy50MoveRule = \board ->
    Board.halfMoveClock board >= 100

expect isDrawBy50MoveRule initialBoard == Bool.false
expect
    board = { initialBoard & flags: initialBoard.flags + 100 }
    isDrawBy50MoveRule board == Bool.true

isDrawByThreefoldRepetition : Board, List Board -> Bool
isDrawByThreefoldRepetition = \board, boardHistory ->
    (List.countIf boardHistory \b -> Board.equalsIgnoreFlags b board) >= 3

expect isDrawByThreefoldRepetition initialBoard [] == Bool.false
expect isDrawByThreefoldRepetition initialBoard [initialBoard, initialBoard, initialBoard] == Bool.true
