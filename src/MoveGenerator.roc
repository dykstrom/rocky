module [generateMoves, withMoves]

import BishopMoveGenerator
import Board exposing [Board, initialBoard]
import Color exposing [Color]
import KingMoveGenerator
import KnightMoveGenerator
import Move exposing [Move]
import PawnMoveGenerator
import RookMoveGenerator
import QueenMoveGenerator

## Generate all pseudo legal moves. This method does not care about checks.
generateMoves : Board, Color -> List Move
generateMoves = \board, sideToMove ->
    myPieces = if sideToMove == White then board.white else board.black
    theirPieces = if sideToMove == White then board.black else board.white

    bishopMoves = BishopMoveGenerator.generateMoves board myPieces theirPieces
    kingMoves = KingMoveGenerator.generateMoves board myPieces theirPieces sideToMove
    knightMoves = KnightMoveGenerator.generateMoves board myPieces theirPieces
    pawnMoves = PawnMoveGenerator.generateMoves board theirPieces
    rookMoves = RookMoveGenerator.generateMoves board myPieces theirPieces
    queenMoves = QueenMoveGenerator.generateMoves board myPieces theirPieces

    bishopMoves
    |> List.concat kingMoves
    |> List.concat knightMoves
    |> List.concat pawnMoves
    |> List.concat rookMoves
    |> List.concat queenMoves

# Initial position
expect
    moves = generateMoves initialBoard White |> toStr
    Set.fromList moves == Set.fromList ["b1a3", "b1c3", "g1f3", "g1h3", "a2a4", "b2b4", "c2c4", "d2d4", "e2e4", "f2f4", "g2g4", "h2h4", "a2a3", "b2b3", "c2c3", "d2d3", "e2e3", "f2f3", "g2g3", "h2h3"]

## Generate all pseudo legal moves for both sides,
## and return the board decorated with those moves.
withMoves : Board -> Board
withMoves = \board ->
    { board & whiteMoves: generateMoves board White, blackMoves: generateMoves board Black }

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

toStr : List Move -> List Str
toStr = \moves ->
    List.map moves \move -> Move.toStr move
