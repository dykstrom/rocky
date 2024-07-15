module [evaluate, illegalCheckValue, checkmateValue, drawValue]

import Board exposing [Bitboard, Board, initialBoard]
import Checker
import Color exposing [Color]
import L
import MoveGenerator
import Num exposing [bitwiseAnd, bitwiseXor]
import Square exposing [a1, f5, h8]
import Util

bishopValue = 3_000
knightValue = 3_000
queenValue = 9_000
pawnValue = 1_000
rookValue = 5_000

checkValue = 100
mobilityValue = 10
illegalCheckValue = -100_000
checkmateValue = -illegalCheckValue
drawValue = 0

## Evaluate the given board and calculate the score for the given color.
## The score will be positive if the given color is in the lead.
evaluate : Board, Color -> I64
evaluate = \board, color ->
    mobilityScore = evaluateMobility board color
    checkScore = evaluateCheck board color
    materialScore = evaluateMaterial board
    mobilityScore + checkScore + (if color == White then materialScore else -materialScore)

# Initial position
expect evaluate initialBoard White == 0
expect evaluate initialBoard Black == 0
# Equal position
expect
    board = Util.withMoves initialBoard ["e2e4", "e7e5"] White
    evaluate board Black == 0
# White is a rook down
expect
    board = { initialBoard &
        white: bitwiseXor initialBoard.white a1,
        rook: bitwiseXor initialBoard.rook a1,
    }
    evaluate board White == -rookValue
# White is checked but not checkmated (he can escape to e2)
expect
    board = Util.withMoves initialBoard ["f2f3", "e7e5", "e2e4", "d8h4"] White
    score = evaluate board Black
    score > 0
# White is checkmated, but we only know that White is checked
expect
    board = Util.withMoves initialBoard ["f2f3", "e7e5", "g2g4", "d8h4"] White
    evaluate board Black > 0

## Return a small positive score if opponent is in check.
evaluateCheck : Board, Color -> I64
evaluateCheck = \board, color ->
    if Checker.isCheck board (Color.flipColor color) then checkValue else 0

evaluateMobility : Board, Color -> I64
evaluateMobility = \board, color ->
    myMoves = MoveGenerator.generateMoves board color
    theirMoves = MoveGenerator.generateMoves board (Color.flipColor color)
    (Num.toI64 (List.len myMoves) - Num.toI64 (List.len theirMoves)) * mobilityValue

## Evaluate material on the board. The score will be positive if White is in the lead.
evaluateMaterial : Board -> I64
evaluateMaterial = \board ->
    (popCount (bitwiseAnd board.white board.bishop) * bishopValue)
    + (popCount (bitwiseAnd board.white board.knight) * knightValue)
    + (popCount (bitwiseAnd board.white board.pawn) * pawnValue)
    + (popCount (bitwiseAnd board.white board.queen) * queenValue)
    + (popCount (bitwiseAnd board.white board.rook) * rookValue)
    - (popCount (bitwiseAnd board.black board.bishop) * bishopValue)
    - (popCount (bitwiseAnd board.black board.knight) * knightValue)
    - (popCount (bitwiseAnd board.black board.pawn) * pawnValue)
    - (popCount (bitwiseAnd board.black board.queen) * queenValue)
    - (popCount (bitwiseAnd board.black board.rook) * rookValue)

popCount : Bitboard -> I64
popCount = \bitboard ->
    Num.toI64 (Num.countOneBits bitboard)

expect popCount 0 == 0
expect popCount 1 == 1
expect popCount 2 == 1
expect popCount 3 == 2
expect popCount 4 == 1
expect popCount 7 == 3
expect popCount h8 == 1
expect popCount (L.or [a1, f5, h8]) == 3
expect popCount initialBoard.white == 16
expect popCount initialBoard.bishop == 4
