module [generateMovesFromSquare, n, ne, e, se, s, sw, w, nw]

import Board exposing [Bitboard, Board, initialBoard]
import L
import Move exposing [Move]
import Num exposing [bitwiseAnd, bitwiseOr]
import Piece exposing [PieceIdx]
import Square exposing [SquareIdx, c6Idx, e4Idx, f4Idx, g4Idx, a1, a2, a3, a4, a5, a6, a7, a8, h1, h2, h3, h4, h5, h6, h7, h8]

n = 8
ne = 9
e = 1
se = -7
s = -8
sw = -9
w = -1
nw = 7

fileA = L.or [a1, a2, a3, a4, a5, a6, a7, a8]
fileH = L.or [h1, h2, h3, h4, h5, h6, h7, h8]

generateMovesFromSquare : Board, Bitboard, Bitboard, SquareIdx, PieceIdx, List I64 -> List Move
generateMovesFromSquare = \board, myPieces, theirPieces, fromIdx, movedIdx, steps ->
    expect
        List.len steps > 0

    iter : I64, I64, Bitboard, List Move -> List Move
    iter = \toIdx, step, blocked, list ->
        if toIdx < 0 || toIdx > 63 then
            list
        else
            toSquare = Square.idxToId (Num.toU64 toIdx)
            if bitwiseAnd toSquare blocked != 0 then
                list
            else if bitwiseAnd toSquare theirPieces != 0 then
                captured = Board.pieceAt board toSquare
                move = Move.createCapture fromIdx (Num.toU64 toIdx) movedIdx captured
                List.append list move
            else
                move = Move.create fromIdx (Num.toU64 toIdx) movedIdx
                iter (toIdx + step) step blocked (List.append list move)

    List.walk steps [] \list, step ->
        blockingFile =
            if step == -7 || step == 1 || step == 9 then
                fileA
            else if step == -9 || step == -1 || step == 7 then
                fileH
            else
                0
        blocked = bitwiseOr myPieces blockingFile
        idx = Num.toI64 fromIdx
        tmp = iter (idx + step) step blocked []
        List.concat list tmp

# Slide east
expect
    moves = runTest Piece.rook g4Idx [e]
    Set.fromList moves == Set.fromList ["g4h4"]
# Slide west
expect
    moves = runTest Piece.rook c6Idx [w]
    Set.fromList moves == Set.fromList ["c6b6", "c6a6"]
# Slide east and west
expect
    moves = runTest Piece.rook e4Idx [e, w]
    Set.fromList moves == Set.fromList ["e4f4", "e4g4", "e4h4", "e4d4", "e4c4", "e4b4", "e4a4"]
# Slide north
expect
    moves = runTest Piece.rook g4Idx [n]
    Set.fromList moves == Set.fromList ["g4g5", "g4g6", "g4g7"]
# Slide south
expect
    moves = runTest Piece.rook c6Idx [s]
    Set.fromList moves == Set.fromList ["c6c5", "c6c4", "c6c3"]
# Slide northeast
expect
    moves = runTest Piece.bishop f4Idx [ne]
    Set.fromList moves == Set.fromList ["f4g5", "f4h6"]
# Slide southeast
expect
    moves = runTest Piece.bishop f4Idx [se]
    Set.fromList moves == Set.fromList ["f4g3"]
# Slide southwest
expect
    moves = runTest Piece.bishop f4Idx [sw]
    Set.fromList moves == Set.fromList ["f4e3"]
# Slide northwest
expect
    moves = runTest Piece.bishop f4Idx [nw]
    Set.fromList moves == Set.fromList ["f4e5", "f4d6", "f4c7"]
# Slide in all directions
expect
    moves = runTest Piece.bishop f4Idx [n, ne, e, se, s, sw, w, nw]
    Set.fromList moves == Set.fromList ["f4f5", "f4f6", "f4f7", "f4g5", "f4h6", "f4g4", "f4h4", "f4g3", "f4f3", "f4e3", "f4e4", "f4d4", "f4c4", "f4b4", "f4a4", "f4e5", "f4d6", "f4c7"]

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

runTest : PieceIdx, SquareIdx, List I64 -> List Str
runTest = \movedIdx, fromIdx, steps ->
    generateMovesFromSquare initialBoard initialBoard.white initialBoard.black fromIdx movedIdx steps
    |> toStr

toStr : List Move -> List Str
toStr = \moves ->
    List.map moves \move -> Move.toStr move
