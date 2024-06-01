module [generateMoves]

import Board exposing [Bitboard, Board, initialBoard]
import Color exposing [Color]
import Move exposing [Move]
import Num exposing [bitwiseAnd]
import Piece
import Square exposing [
    SquareIdx,
    a1Idx,
    b1Idx,
    c1Idx,
    d1Idx,
    e1Idx,
    f1Idx,
    g1Idx,
    h1Idx,
    a2Idx,
    b2Idx,
    c2Idx,
    d2Idx,
    e2Idx,
    f2Idx,
    g2Idx,
    h2Idx,
    a3Idx,
    b3Idx,
    c3Idx,
    d3Idx,
    e3Idx,
    f3Idx,
    g3Idx,
    h3Idx,
    a4Idx,
    b4Idx,
    c4Idx,
    d4Idx,
    e4Idx,
    f4Idx,
    g4Idx,
    h4Idx,
    a5Idx,
    b5Idx,
    c5Idx,
    d5Idx,
    e5Idx,
    f5Idx,
    g5Idx,
    h5Idx,
    a6Idx,
    b6Idx,
    c6Idx,
    d6Idx,
    e6Idx,
    f6Idx,
    g6Idx,
    h6Idx,
    a7Idx,
    b7Idx,
    c7Idx,
    d7Idx,
    e7Idx,
    f7Idx,
    g7Idx,
    h7Idx,
    a8Idx,
    b8Idx,
    c8Idx,
    d8Idx,
    e8Idx,
    f8Idx,
    g8Idx,
    h8Idx,
]
import Util

## All squares a king can move to from a certain square
kingSquares = Dict.fromList [
    # Rank 1
    (a1Idx, [a2Idx, b2Idx, b1Idx]),
    (b1Idx, [a1Idx, a2Idx, b2Idx, c2Idx, c1Idx]),
    (c1Idx, [b1Idx, b2Idx, c2Idx, d2Idx, d1Idx]),
    (d1Idx, [c1Idx, c2Idx, d2Idx, e2Idx, e1Idx]),
    (e1Idx, [d1Idx, d2Idx, e2Idx, f2Idx, f1Idx]),
    (f1Idx, [e1Idx, e2Idx, f2Idx, g2Idx, g1Idx]),
    (g1Idx, [f1Idx, f2Idx, g2Idx, h2Idx, h1Idx]),
    (h1Idx, [g1Idx, g2Idx, h2Idx]),

    # Rank 2
    (a2Idx, [a3Idx, b3Idx, b2Idx, b1Idx, a1Idx]),
    (b2Idx, [a2Idx, a3Idx, b3Idx, c3Idx, c2Idx, c1Idx, b1Idx, a1Idx]),
    (c2Idx, [b2Idx, b3Idx, c3Idx, d3Idx, d2Idx, d1Idx, c1Idx, b1Idx]),
    (d2Idx, [c2Idx, c3Idx, d3Idx, e3Idx, e2Idx, e1Idx, d1Idx, c1Idx]),
    (e2Idx, [d2Idx, d3Idx, e3Idx, f3Idx, f2Idx, f1Idx, e1Idx, d1Idx]),
    (f2Idx, [e2Idx, e3Idx, f3Idx, g3Idx, g2Idx, g1Idx, f1Idx, e1Idx]),
    (g2Idx, [f2Idx, f3Idx, g3Idx, h3Idx, h2Idx, h1Idx, g1Idx, f1Idx]),
    (h2Idx, [g2Idx, g3Idx, h3Idx, h1Idx, g1Idx]),

    # Rank 3
    (a3Idx, [a4Idx, b4Idx, b3Idx, b2Idx, a2Idx]),
    (b3Idx, [a3Idx, a4Idx, b4Idx, c4Idx, c3Idx, c2Idx, b2Idx, a2Idx]),
    (c3Idx, [b3Idx, b4Idx, c4Idx, d4Idx, d3Idx, d2Idx, c2Idx, b2Idx]),
    (d3Idx, [c3Idx, c4Idx, d4Idx, e4Idx, e3Idx, e2Idx, d2Idx, c2Idx]),
    (e3Idx, [d3Idx, d4Idx, e4Idx, f4Idx, f3Idx, f2Idx, e2Idx, d2Idx]),
    (f3Idx, [e3Idx, e4Idx, f4Idx, g4Idx, g3Idx, g2Idx, f2Idx, e2Idx]),
    (g3Idx, [f3Idx, f4Idx, g4Idx, h4Idx, h3Idx, h2Idx, g2Idx, f2Idx]),
    (h3Idx, [g3Idx, g4Idx, h4Idx, h2Idx, g2Idx]),

    # Rank 4
    (a4Idx, [a5Idx, b5Idx, b4Idx, b3Idx, a3Idx]),
    (b4Idx, [a4Idx, a5Idx, b5Idx, c5Idx, c4Idx, c3Idx, b3Idx, a3Idx]),
    (c4Idx, [b4Idx, b5Idx, c5Idx, d5Idx, d4Idx, d3Idx, c3Idx, b3Idx]),
    (d4Idx, [c4Idx, c5Idx, d5Idx, e5Idx, e4Idx, e3Idx, d3Idx, c3Idx]),
    (e4Idx, [d4Idx, d5Idx, e5Idx, f5Idx, f4Idx, f3Idx, e3Idx, d3Idx]),
    (f4Idx, [e4Idx, e5Idx, f5Idx, g5Idx, g4Idx, g3Idx, f3Idx, e3Idx]),
    (g4Idx, [f4Idx, f5Idx, g5Idx, h5Idx, h4Idx, h3Idx, g3Idx, f3Idx]),
    (h4Idx, [g4Idx, g5Idx, h5Idx, h3Idx, g3Idx]),

    # Rank 5
    (a5Idx, [a6Idx, b6Idx, b5Idx, b4Idx, a4Idx]),
    (b5Idx, [a5Idx, a6Idx, b6Idx, c6Idx, c5Idx, c4Idx, b4Idx, a4Idx]),
    (c5Idx, [b5Idx, b6Idx, c6Idx, d6Idx, d5Idx, d4Idx, c4Idx, b4Idx]),
    (d5Idx, [c5Idx, c6Idx, d6Idx, e6Idx, e5Idx, e4Idx, d4Idx, c4Idx]),
    (e5Idx, [d5Idx, d6Idx, e6Idx, f6Idx, f5Idx, f4Idx, e4Idx, d4Idx]),
    (f5Idx, [e5Idx, e6Idx, f6Idx, g6Idx, g5Idx, g4Idx, f4Idx, e4Idx]),
    (g5Idx, [f5Idx, f6Idx, g6Idx, h6Idx, h5Idx, h4Idx, g4Idx, f4Idx]),
    (h5Idx, [g5Idx, g6Idx, h6Idx, h4Idx, g4Idx]),

    # Rank 6
    (a6Idx, [a7Idx, b7Idx, b6Idx, b5Idx, a5Idx]),
    (b6Idx, [a6Idx, a7Idx, b7Idx, c7Idx, c6Idx, c5Idx, b5Idx, a5Idx]),
    (c6Idx, [b6Idx, b7Idx, c7Idx, d7Idx, d6Idx, d5Idx, c5Idx, b5Idx]),
    (d6Idx, [c6Idx, c7Idx, d7Idx, e7Idx, e6Idx, e5Idx, d5Idx, c5Idx]),
    (e6Idx, [d6Idx, d7Idx, e7Idx, f7Idx, f6Idx, f5Idx, e5Idx, d5Idx]),
    (f6Idx, [e6Idx, e7Idx, f7Idx, g7Idx, g6Idx, g5Idx, f5Idx, e5Idx]),
    (g6Idx, [f6Idx, f7Idx, g7Idx, h7Idx, h6Idx, h5Idx, g5Idx, f5Idx]),
    (h6Idx, [g6Idx, g7Idx, h7Idx, h5Idx, g5Idx]),

    # Rank 7
    (a7Idx, [a8Idx, b8Idx, b7Idx, b6Idx, a6Idx]),
    (b7Idx, [a7Idx, a8Idx, b8Idx, c8Idx, c7Idx, c6Idx, b6Idx, a6Idx]),
    (c7Idx, [b7Idx, b8Idx, c8Idx, d8Idx, d7Idx, d6Idx, c6Idx, b6Idx]),
    (d7Idx, [c7Idx, c8Idx, d8Idx, e8Idx, e7Idx, e6Idx, d6Idx, c6Idx]),
    (e7Idx, [d7Idx, d8Idx, e8Idx, f8Idx, f7Idx, f6Idx, e6Idx, d6Idx]),
    (f7Idx, [e7Idx, e8Idx, f8Idx, g8Idx, g7Idx, g6Idx, f6Idx, e6Idx]),
    (g7Idx, [f7Idx, f8Idx, g8Idx, h8Idx, h7Idx, h6Idx, g6Idx, f6Idx]),
    (h7Idx, [g7Idx, g8Idx, h8Idx, h6Idx, g6Idx]),

    # Rank 8
    (a8Idx, [b8Idx, b7Idx, a7Idx]),
    (b8Idx, [a8Idx, c8Idx, c7Idx, b7Idx, a7Idx]),
    (c8Idx, [b8Idx, d8Idx, d7Idx, c7Idx, b7Idx]),
    (d8Idx, [c8Idx, e8Idx, e7Idx, d7Idx, c7Idx]),
    (e8Idx, [d8Idx, f8Idx, f7Idx, e7Idx, d7Idx]),
    (f8Idx, [e8Idx, g8Idx, g7Idx, f7Idx, e7Idx]),
    (g8Idx, [f8Idx, h8Idx, h7Idx, g7Idx, f7Idx]),
    (h8Idx, [g8Idx, h7Idx, g7Idx]),
]

## Castling moves
e1c1 = Move.createCastling e1Idx c1Idx
e1g1 = Move.createCastling e1Idx g1Idx
e8c8 = Move.createCastling e8Idx c8Idx
e8g8 = Move.createCastling e8Idx g8Idx

generateMoves : Board, Bitboard, Bitboard, Color -> List Move
generateMoves = \board, myPieces, theirPieces, sideToMove ->
    # Find from squares
    kings = bitwiseAnd myPieces board.king
    kingIdxs = Board.bbToIdxs kings
    when List.first kingIdxs is
        Ok idx ->
            generateMovesFromSquare board myPieces theirPieces idx
            |> List.concat (generateCastlingMoves board idx sideToMove)

        Err _ -> crash "Should not happen: king not found"

expect generateMoves initialBoard initialBoard.white initialBoard.black White == []

generateCastlingMoves : Board, SquareIdx, Color -> List Move
generateCastlingMoves = \board, fromIdx, sideToMove ->
    if sideToMove == White && fromIdx == e1Idx then
        if Board.isCastlingAllowed board e1Idx c1Idx then
            if Board.isCastlingAllowed board e1Idx g1Idx then
                [e1c1, e1g1]
            else
                [e1c1]
        else if Board.isCastlingAllowed board e1Idx g1Idx then
            [e1g1]
        else
            [] # Castling not allowed for White
    else if sideToMove == Black && fromIdx == e8Idx then
        if Board.isCastlingAllowed board e8Idx c8Idx then
            if Board.isCastlingAllowed board e8Idx g8Idx then
                [e8c8, e8g8]
            else
                [e8c8]
        else if Board.isCastlingAllowed board e8Idx g8Idx then
            [e8g8]
        else
            [] # Castling not allowed for Black
    else
        [] # King not on original square

expect
    board = Util.withMoves initialBoard ["e2e4", "e7e5", "g1f3", "g8f6", "f1c4", "f8c5"] White
    moves = generateCastlingMoves board e1Idx White |> toStr
    Set.fromList moves == Set.fromList ["e1g1"]

generateMovesFromSquare : Board, Bitboard, Bitboard, SquareIdx -> List Move
generateMovesFromSquare = \board, myPieces, theirPieces, fromIdx ->
    when Dict.get kingSquares fromIdx is
        Ok toIdxs ->
            # Remove squares occupied by my pieces and create moves
            toIdxs
            |> List.keepIf \toIdx ->
                toId = Square.idxToId toIdx
                bitwiseAnd myPieces toId == 0
            |> List.map \toIdx ->
                createMove board theirPieces fromIdx toIdx

        Err _ -> crash "Should not happen: fromIdx not found: $(Num.toStr fromIdx)"

# After 1. e4 e5 2. Bc4 Bc5
expect
    board = Util.withMoves initialBoard ["e2e4", "e7e5", "f1c4", "f8c5"] White
    moves = generateMovesFromSquare board board.white board.black e1Idx |> toStr
    Set.fromList moves == Set.fromList ["e1e2", "e1f1"]

createMove : Board, Bitboard, SquareIdx, SquareIdx -> Move
createMove = \board, theirPieces, fromIdx, toIdx ->
    toId = Square.idxToId toIdx
    if bitwiseAnd theirPieces toId != 0 then
        captured = Board.pieceAt board toId
        Move.createCapture fromIdx toIdx Piece.king captured
    else
        Move.create fromIdx toIdx Piece.king

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

toStr : List Move -> List Str
toStr = \moves ->
    List.map moves \move -> Move.toStr move
