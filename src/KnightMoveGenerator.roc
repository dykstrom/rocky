module [generateMoves]

import Board exposing [Bitboard, Board, initialBoard]
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

## All squares a knight can move to from a certain square
knightSquares = Dict.fromList [
    # Rank 1
    (a1Idx, [b3Idx, c2Idx]),
    (b1Idx, [a3Idx, c3Idx, d2Idx]),
    (c1Idx, [a2Idx, b3Idx, d3Idx, e2Idx]),
    (d1Idx, [b2Idx, c3Idx, e3Idx, f2Idx]),
    (e1Idx, [c2Idx, d3Idx, f3Idx, g2Idx]),
    (f1Idx, [d2Idx, e3Idx, g3Idx, h2Idx]),
    (g1Idx, [e2Idx, f3Idx, h3Idx]),
    (h1Idx, [f2Idx, g3Idx]),

    # Rank 2
    (a2Idx, [b4Idx, c3Idx, c1Idx]),
    (b2Idx, [a4Idx, c4Idx, d3Idx, d1Idx]),
    (c2Idx, [a1Idx, a3Idx, b4Idx, d4Idx, e3Idx, e1Idx]),
    (d2Idx, [b1Idx, b3Idx, c4Idx, e4Idx, f3Idx, f1Idx]),
    (e2Idx, [c1Idx, c3Idx, d4Idx, f4Idx, g3Idx, g1Idx]),
    (f2Idx, [d1Idx, d3Idx, e4Idx, g4Idx, h3Idx, h1Idx]),
    (g2Idx, [e1Idx, e3Idx, f4Idx, h4Idx]),
    (h2Idx, [f1Idx, f3Idx, g4Idx]),

    # Rank 3
    (a3Idx, [b5Idx, c4Idx, c2Idx, b1Idx]),
    (b3Idx, [a1Idx, a5Idx, c5Idx, d4Idx, d2Idx, c1Idx]),
    (c3Idx, [b1Idx, a2Idx, a4Idx, b5Idx, d5Idx, e4Idx, e2Idx, d1Idx]),
    (d3Idx, [c1Idx, b2Idx, b4Idx, c5Idx, e5Idx, f4Idx, f2Idx, e1Idx]),
    (e3Idx, [d1Idx, c2Idx, c4Idx, d5Idx, f5Idx, g4Idx, g2Idx, f1Idx]),
    (f3Idx, [e1Idx, d2Idx, d4Idx, e5Idx, g5Idx, h4Idx, h2Idx, g1Idx]),
    (g3Idx, [f1Idx, e2Idx, e4Idx, f5Idx, h5Idx, h1Idx]),
    (h3Idx, [g1Idx, f2Idx, f4Idx, g5Idx]),

    # Rank 4
    (a4Idx, [b6Idx, c5Idx, c3Idx, b2Idx]),
    (b4Idx, [a2Idx, a6Idx, c6Idx, d5Idx, d3Idx, c2Idx]),
    (c4Idx, [b2Idx, a3Idx, a5Idx, b6Idx, d6Idx, e5Idx, e3Idx, d2Idx]),
    (d4Idx, [c2Idx, b3Idx, b5Idx, c6Idx, e6Idx, f5Idx, f3Idx, e2Idx]),
    (e4Idx, [d2Idx, c3Idx, c5Idx, d6Idx, f6Idx, g5Idx, g3Idx, f2Idx]),
    (f4Idx, [e2Idx, d3Idx, d5Idx, e6Idx, g6Idx, h5Idx, h3Idx, g2Idx]),
    (g4Idx, [f2Idx, e3Idx, e5Idx, f6Idx, h6Idx, h2Idx]),
    (h4Idx, [g2Idx, f3Idx, f5Idx, g6Idx]),

    # Rank 5
    (a5Idx, [b7Idx, c6Idx, c4Idx, b3Idx]),
    (b5Idx, [a3Idx, a7Idx, c7Idx, d6Idx, d4Idx, c3Idx]),
    (c5Idx, [b3Idx, a4Idx, a6Idx, b7Idx, d7Idx, e6Idx, e4Idx, d3Idx]),
    (d5Idx, [c3Idx, b4Idx, b6Idx, c7Idx, e7Idx, f6Idx, f4Idx, e3Idx]),
    (e5Idx, [d3Idx, c4Idx, c6Idx, d7Idx, f7Idx, g6Idx, g4Idx, f3Idx]),
    (f5Idx, [e3Idx, d4Idx, d6Idx, e7Idx, g7Idx, h6Idx, h4Idx, g3Idx]),
    (g5Idx, [f3Idx, e4Idx, e6Idx, f7Idx, h7Idx, h3Idx]),
    (h5Idx, [g3Idx, f4Idx, f6Idx, g7Idx]),

    # Rank 6
    (a6Idx, [b8Idx, c7Idx, c5Idx, b4Idx]),
    (b6Idx, [a4Idx, a8Idx, c8Idx, d7Idx, d5Idx, c4Idx]),
    (c6Idx, [b4Idx, a5Idx, a7Idx, b8Idx, d8Idx, e7Idx, e5Idx, d4Idx]),
    (d6Idx, [c4Idx, b5Idx, b7Idx, c8Idx, e8Idx, f7Idx, f5Idx, e4Idx]),
    (e6Idx, [d4Idx, c5Idx, c7Idx, d8Idx, f8Idx, g7Idx, g5Idx, f4Idx]),
    (f6Idx, [e4Idx, d5Idx, d7Idx, e8Idx, g8Idx, h7Idx, h5Idx, g4Idx]),
    (g6Idx, [f4Idx, e5Idx, e7Idx, f8Idx, h8Idx, h4Idx]),
    (h6Idx, [g4Idx, f5Idx, f7Idx, g8Idx]),

    # Rank 7
    (a7Idx, [c8Idx, c6Idx, b5Idx]),
    (b7Idx, [d8Idx, d6Idx, c5Idx, a5Idx]),
    (c7Idx, [b5Idx, a6Idx, a8Idx, e8Idx, e6Idx, d5Idx]),
    (d7Idx, [c5Idx, b6Idx, b8Idx, f8Idx, f6Idx, e5Idx]),
    (e7Idx, [d5Idx, c6Idx, c8Idx, g8Idx, g6Idx, f5Idx]),
    (f7Idx, [e5Idx, d6Idx, d8Idx, h8Idx, h6Idx, g5Idx]),
    (g7Idx, [f5Idx, e6Idx, e8Idx, h5Idx]),
    (h7Idx, [g5Idx, f6Idx, f8Idx]),

    # Rank 8
    (a8Idx, [c7Idx, b6Idx]),
    (b8Idx, [a6Idx, d7Idx, c6Idx]),
    (c8Idx, [b6Idx, a7Idx, e7Idx, d6Idx]),
    (d8Idx, [c6Idx, b7Idx, f7Idx, e6Idx]),
    (e8Idx, [d6Idx, c7Idx, g7Idx, f6Idx]),
    (f8Idx, [e6Idx, d7Idx, h7Idx, g6Idx]),
    (g8Idx, [f6Idx, e7Idx, h6Idx]),
    (h8Idx, [g6Idx, f7Idx]),
]

generateMoves : Board, Bitboard, Bitboard -> List Move
generateMoves = \board, myPieces, theirPieces ->
    # Find from squares
    knights = bitwiseAnd myPieces board.knight
    knightIdxs = Board.bbToIdxs knights
    # For each from square, create a knight move
    List.walk knightIdxs [] \list, idx ->
        List.concat list (generateMovesFromSquare board myPieces theirPieces idx)

expect
    moves = generateMoves initialBoard initialBoard.white initialBoard.black |> toStr
    Set.fromList moves == Set.fromList ["b1a3", "b1c3", "g1f3", "g1h3"]

generateMovesFromSquare : Board, Bitboard, Bitboard, SquareIdx -> List Move
generateMovesFromSquare = \board, myPieces, theirPieces, fromIdx ->
    when Dict.get knightSquares fromIdx is
        Ok toIdxs ->
            # Remove squares occupied by my pieces and create moves
            toIdxs
            |> List.keepIf \toIdx ->
                toId = Square.idxToId toIdx
                bitwiseAnd myPieces toId == 0
            |> List.map \toIdx ->
                createMove board theirPieces fromIdx toIdx

        Err _ -> crash "Should not happen: fromIdx not found: $(Num.toStr fromIdx)"

# Initial position White
expect
    moves = generateMovesFromSquare initialBoard initialBoard.white initialBoard.black g1Idx |> toStr
    Set.fromList moves == Set.fromList ["g1f3", "g1h3"]
# Initial position Black
expect
    moves = generateMovesFromSquare initialBoard initialBoard.black initialBoard.white b8Idx |> toStr
    Set.fromList moves == Set.fromList ["b8a6", "b8c6"]

createMove : Board, Bitboard, SquareIdx, SquareIdx -> Move
createMove = \board, theirPieces, fromIdx, toIdx ->
    toId = Square.idxToId toIdx
    if bitwiseAnd theirPieces toId != 0 then
        captured = Board.pieceAt board toId
        Move.createCapture fromIdx toIdx Piece.knight captured
    else
        Move.create fromIdx toIdx Piece.knight

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

toStr : List Move -> List Str
toStr = \moves ->
    List.map moves \move -> Move.toStr move
