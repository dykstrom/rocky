module [withMoves, withHistory]

import Board exposing [Board, initialBoard]
import Color exposing [Color]
import Move exposing [Move]
import Piece exposing [PieceIdx]
import S
import Square exposing [SquareIdx, c1Idx, c8Idx, e1Idx, e8Idx, g1Idx, g8Idx]

## Setup the given board by making the given moves.
withMoves : Board, List Str, Color -> Board
withMoves = \board, moves, sideToMove ->
    List.walk moves { b: board, c: sideToMove } \state, str ->
        move = parseMove state.b str
        newBoard = Board.makeMove state.b move state.c
        newColor = Color.flipColor state.c
        { b: newBoard, c: newColor }
    |> \state -> state.b

expect withMoves initialBoard ["g1f3", "g8f6", "f3g1", "f6g8"] White == { initialBoard & flags: initialBoard.flags + 4 }

## Setup the given board by making the given moves. Return the board and the board history.
withHistory : Board, List Str, Color -> { board : Board, history : List Board }
withHistory = \board, moves, sideToMove ->
    List.walk moves { b: board, h: [], c: sideToMove } \state, str ->
        move = parseMove state.b str
        newBoard = Board.makeMove state.b move state.c
        newColor = Color.flipColor state.c
        { b: newBoard, h: List.append state.h newBoard, c: newColor }
    |> \state -> { board: state.b, history: state.h }

## Parse and return the given move string. This is a simplified parse method to be ued in tests only.
## It can only handle legal moves. We cannot use the MoveParser to parse moves, because that would
## cause an illegal import cycle.
parseMove : Board, Str -> Move
parseMove = \board, str ->
    fromIdx =
        str
        |> S.substr { start: 0, len: 2 }
        |> Result.try Square.fromStr
        |> Result.withDefault 0
    toIdx =
        str
        |> S.substr { start: 2, len: 2 }
        |> Result.try Square.fromStr
        |> Result.withDefault 0
    promotedIdx =
        str
        |> \s -> if S.len s == 5 then S.substr s { start: 4, len: 1 } else Err NotFound
        |> Result.try Piece.fromStr
        |> Result.withDefault 0
    movedIdx = Board.pieceAt board (Square.idxToId fromIdx)
    capturedIdx = Board.pieceAt board (Square.idxToId toIdx)

    if isEnPassant board toIdx movedIdx then
        Move.createCastling fromIdx toIdx
    else if isCastling fromIdx toIdx movedIdx then
        Move.createCastling fromIdx toIdx
    else if capturedIdx != 0 && promotedIdx != 0 then
        Move.createCapturePromotion fromIdx toIdx capturedIdx promotedIdx
    else if capturedIdx != 0 then
        Move.createCapture fromIdx toIdx movedIdx capturedIdx
    else if promotedIdx != 0 then
        Move.createPromotion fromIdx toIdx promotedIdx
    else
        Move.create fromIdx toIdx movedIdx

expect parseMove initialBoard "e2e4" == Move.e2e4
expect parseMove initialBoard "e1g1" == Move.e1g1

isEnPassant : Board, SquareIdx, PieceIdx -> Bool
isEnPassant = \board, toIdx, movedIdx ->
    (movedIdx == Piece.pawn)
    && (Board.isEnPassantAllowed board)
    && (Board.enPassantSquare board == toIdx)

isCastling : SquareIdx, SquareIdx, PieceIdx -> Bool
isCastling = \fromIdx, toIdx, movedIdx ->
    (movedIdx == Piece.king)
    &&
    ((fromIdx == e1Idx && toIdx == c1Idx) || (fromIdx == e1Idx && toIdx == g1Idx) || (fromIdx == e8Idx && toIdx == c8Idx) || (fromIdx == e8Idx && toIdx == g8Idx))
