module [findMove]

import Board exposing [Board, initialBoard]
import Checker
import Color exposing [Color]
import Evaluator exposing [illegalCheckValue]
import MoveGenerator
import Move exposing [Move]
import Util

findMove : Board, List Board, Color -> [Draw, Mated, FoundMove { move : Move, score : I64 }]
findMove = \board, boardHistory, sideToMove ->

    # Generate all pseudo legal moves
    moves = MoveGenerator.generateMoves board sideToMove

    # Score the moves
    scoredMoves = List.map moves \move ->
        newBoard = makeMove board move sideToMove
        score = Evaluator.evaluate newBoard boardHistory sideToMove
        { move, score }

    # Sort them according to score
    sortedMoves = List.sortWith scoredMoves compareMoves
    # dbg format (List.sublist sortedMoves { start: 0, len: 5 })

    # If the best move is illegalCheckValue we have been mated
    # If there are no moves, and we are in check, we have been mated
    # If there are no moves, and we are not in check, it is a draw
    when List.get sortedMoves 0 is
        Ok { move: move, score: score } ->
            if score == illegalCheckValue then Mated else FoundMove { move: move, score: score }

        Err OutOfBounds ->
            if Checker.isCheck board (Color.flipColor sideToMove) then Mated else Draw

expect
    { board, history } = Util.withHistory initialBoard ["e2e4", "d7d5"] White
    when findMove board history White is
        FoundMove { move: move, score: _ } -> move == Move.e4d5
        _ -> Bool.false

compareMoves : { move : Move, score : I64 }, { move : Move, score : I64 } -> [LT, EQ, GT]
compareMoves = \m1, m2 ->
    if m1.score > m2.score then
        LT
    else if m2.score > m1.score then
        GT
    else
        EQ

makeMove : Board, Move, Color -> Board
makeMove = \board, move, color ->
    Board.makeMove board move color

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

format = \list ->
    List.map list \{ move, score } ->
        { move: Move.toStr move, score: score }

expect format [] == []
