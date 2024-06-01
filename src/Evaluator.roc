module [evaluate, illegalCheckValue]

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
drawValue = 0

# Evaluate the given board and calculate the score for the side that just moved.
# The score will be positive if the side that moved is leading.
evaluate : Board, List Board, Color -> I64
evaluate = \board, boardHistory, sideThatMoved ->
    if Checker.isCheck board (Color.flipColor sideThatMoved) then
        illegalCheckValue
    else if isDrawBy50MoveRule board then
        drawValue
    else if isDrawByThreefoldRepetition board boardHistory then
        drawValue
    else
        mobilityScore = evaluateMobility board sideThatMoved
        checkScore = evaluateCheck board sideThatMoved
        materialScore = evaluateMaterial board
        mobilityScore + checkScore + (if sideThatMoved == White then materialScore else -materialScore)

# Initial position
expect Evaluator.evaluate initialBoard [] White == 0
expect Evaluator.evaluate initialBoard [] Black == 0
# Without a white rook
expect
    board = { initialBoard &
        white: bitwiseXor initialBoard.white a1,
        rook: bitwiseXor initialBoard.rook a1,
    }
    Evaluator.evaluate board [] White == -rookValue
# Side that moved is checked, that is, the sideThatMoved
# is still in check after the latest move
expect
    { board, history } = Util.withHistory initialBoard ["f2f3", "e7e5", "g2g4", "d8h4", "e2e4"] White
    Evaluator.evaluate board history White == illegalCheckValue
# Threefold repetition of position
expect
    { board, history } = Util.withHistory initialBoard ["g1f3", "g8f6", "f3g1", "f6g8", "g1f3", "g8f6", "f3g1", "f6g8", "g1f3"] White
    Evaluator.evaluate board history White == drawValue

## Return a small positive score if opponent is in check.
evaluateCheck : Board, Color -> I64
evaluateCheck = \board, sideThatMoved ->
    if Checker.isCheck board sideThatMoved then checkValue else 0

evaluateMobility : Board, Color -> I64
evaluateMobility = \board, sideThatMoved ->
    myMoves = MoveGenerator.generateMoves board sideThatMoved
    theirMoves = MoveGenerator.generateMoves board (Color.flipColor sideThatMoved)
    (Num.toI64 (List.len myMoves) - Num.toI64 (List.len theirMoves)) * mobilityValue

# Evaluate material on the board. The score will be positive if White is leading.
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
    iter = \bb, sum ->
        if bb == 0 then
            sum
        else
            iter (bitwiseAnd bb (bb - 1)) (sum + 1)
    iter bitboard 0

expect Evaluator.popCount 0 == 0
expect Evaluator.popCount 1 == 1
expect Evaluator.popCount 2 == 1
expect Evaluator.popCount 3 == 2
expect Evaluator.popCount 4 == 1
expect Evaluator.popCount 7 == 3
expect Evaluator.popCount h8 == 1
expect Evaluator.popCount (L.or [a1, f5, h8]) == 3

isDrawBy50MoveRule : Board -> Bool
isDrawBy50MoveRule = \board ->
    Board.halfMoveClock board >= 100

expect isDrawBy50MoveRule initialBoard == Bool.false
expect
    board = { initialBoard & flags: initialBoard.flags + 100 }
    isDrawBy50MoveRule board == Bool.true

isDrawByThreefoldRepetition : Board, List Board -> Bool
isDrawByThreefoldRepetition = \board, boardHistory ->
    (List.countIf boardHistory \b -> Board.equalsIgnoreFlags b board) >= 2

expect isDrawByThreefoldRepetition initialBoard [] == Bool.false
expect isDrawByThreefoldRepetition initialBoard [initialBoard, initialBoard] == Bool.true
