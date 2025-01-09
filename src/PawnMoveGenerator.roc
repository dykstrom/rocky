module [generateMoves]

import Board exposing [Bitboard, Board, initialBoard]
import Fen
import FenParser
import L
import Move exposing [Move]
import Num exposing [bitwiseAnd, bitwiseNot, bitwiseOr, shiftLeftBy, shiftRightZfBy]
import Piece
import Square exposing [SquareIdx, a8Idx, h1Idx, a1, h1, a2, b2, c2, d2, e2, f2, g2, h2, a3, h3, a4, h4, a5, h5, a6, h6, a7, b7, c7, d7, e7, f7, g7, h7, a8, h8]
import Util

rank2 = L.or [a2, b2, c2, d2, e2, f2, g2, h2]
rank7 = L.or [a7, b7, c7, d7, e7, f7, g7, h7]

filesAG = bitwiseNot (L.or [h1, h2, h3, h4, h5, h6, h7, h8])
filesBH = bitwiseNot (L.or [a1, a2, a3, a4, a5, a6, a7, a8])

promotionPieces = [Piece.bishop, Piece.knight, Piece.queen, Piece.rook]

generateMoves : Board, Bitboard -> List Move
generateMoves = \board, theirPieces ->
    if bitwiseAnd board.white theirPieces == 0 then
        generateWhiteMoves board theirPieces
    else
        generateBlackMoves board theirPieces

expect
    board = FenParser.fenToBoard Fen.whiteCanPromote
    moves = generateMoves board board.black |> toStr
    Set.fromList moves == Set.fromList ["b2b4", "c2c4", "e2e4", "f2f4", "g2g4", "h2h4", "b2a3", "b2b3", "c2c3", "e2e3", "f2f3", "g2g3", "h2h3", "e7d8b", "e7d8n", "e7d8q", "e7d8r", "e7f8b", "e7f8n", "e7f8q", "e7f8r"]
expect
    board = FenParser.fenToBoard Fen.blackCanCaptureEp
    moves = generateMoves board board.white |> toStr
    Set.fromList moves == Set.fromList ["b7b5", "c7c5", "e7e5", "f7f5", "g7g5", "h7h5", "b7b6", "c7c6", "e7e6", "f7f6", "g7g6", "h7h6", "a4a3", "a4b3", "c7d6", "e7d6"]

generateWhiteMoves : Board, Bitboard -> List Move
generateWhiteMoves = \board, theirPieces ->
    occupied = bitwiseOr board.white board.black
    free = bitwiseNot occupied
    myPawns = bitwiseAnd board.white board.pawn

    # First moves
    pawnsOnRank2 = bitwiseAnd myPawns rank2
    fmFromSquares = bitwiseAnd pawnsOnRank2 (shiftRightZfBy free 8)
    fmToSquares = bitwiseAnd (shiftLeftBy fmFromSquares 16) free
    fmToIdxs = Board.bbToIdxs fmToSquares
    fmMoves = List.map fmToIdxs \toIdx ->
        Move.create (toIdx - 16) toIdx Piece.pawn

    # Normal moves
    nmToSquares = bitwiseAnd (shiftLeftBy myPawns 8) free
    nmToIdxs = Board.bbToIdxs nmToSquares
    nmMoves =
        List.keepIf nmToIdxs isNormalRank
        |> List.map \toIdx -> Move.create (toIdx - 8) toIdx Piece.pawn

    # Promotion moves
    prMoves = createPromotionMoves nmToIdxs (\toIdx -> toIdx - 8)

    # Capture right
    crToSquares = bitwiseAnd (bitwiseAnd (shiftLeftBy myPawns 9) filesBH) theirPieces
    crToIdxs = Board.bbToIdxs crToSquares
    crMoves =
        List.keepIf crToIdxs isNormalRank
        |> List.map \toIdx ->
            captured = Board.pieceAt board (Square.idxToId toIdx)
            Move.createCapture (toIdx - 9) toIdx Piece.pawn captured

    # Capture right and promote
    crpMoves = createCapturePromotionMoves board crToIdxs (\toIdx -> toIdx - 9)

    # Capture left
    clToSquares = bitwiseAnd (bitwiseAnd (shiftLeftBy myPawns 7) filesAG) theirPieces
    clToIdxs = Board.bbToIdxs clToSquares
    clMoves =
        List.keepIf clToIdxs isNormalRank
        |> List.map \toIdx ->
            captured = Board.pieceAt board (Square.idxToId toIdx)
            Move.createCapture (toIdx - 7) toIdx Piece.pawn captured

    # Capture left and promote
    clpMoves = createCapturePromotionMoves board clToIdxs (\toIdx -> toIdx - 7)

    # 'En passant' captures
    epMoves = if Board.isEnPassantAllowed board then createWhiteEnPassantMoves board myPawns else []

    fmMoves
    |> List.concat nmMoves
    |> List.concat prMoves
    |> List.concat crMoves
    |> List.concat crpMoves
    |> List.concat clMoves
    |> List.concat clpMoves
    |> List.concat epMoves

# Initial position
expect
    moves = generateWhiteMoves initialBoard initialBoard.black |> toStr
    Set.fromList moves == Set.fromList ["a2a4", "b2b4", "c2c4", "d2d4", "e2e4", "f2f4", "g2g4", "h2h4", "a2a3", "b2b3", "c2c3", "d2d3", "e2e3", "f2f3", "g2g3", "h2h3"]
# After 1. e4 d5
expect
    board = Util.withMoves initialBoard ["e2e4", "d7d5"] White
    moves = generateWhiteMoves board board.black |> toStr
    Set.fromList moves == Set.fromList ["a2a4", "b2b4", "c2c4", "d2d4", "f2f4", "g2g4", "h2h4", "a2a3", "b2b3", "c2c3", "d2d3", "f2f3", "g2g3", "h2h3", "e4e5", "e4d5"]

generateBlackMoves : Board, Bitboard -> List Move
generateBlackMoves = \board, theirPieces ->
    occupied = bitwiseOr board.white board.black
    free = bitwiseNot occupied
    myPawns = bitwiseAnd board.black board.pawn

    # First moves
    pawnsOnRank7 = bitwiseAnd myPawns rank7
    fmFromSquares = bitwiseAnd pawnsOnRank7 (shiftLeftBy free 8)
    fmToSquares = bitwiseAnd (shiftRightZfBy fmFromSquares 16) free
    fmToIdxs = Board.bbToIdxs fmToSquares
    fmMoves = List.map fmToIdxs \toIdx ->
        Move.create (toIdx + 16) toIdx Piece.pawn

    # Normal moves
    nmToSquares = bitwiseAnd (shiftRightZfBy myPawns 8) free
    nmToIdxs = Board.bbToIdxs nmToSquares
    nmMoves =
        List.keepIf nmToIdxs isNormalRank
        |> List.map \toIdx -> Move.create (toIdx + 8) toIdx Piece.pawn

    # Promotion moves
    prMoves = createPromotionMoves nmToIdxs (\toIdx -> toIdx + 8)

    # Capture right
    crToSquares = bitwiseAnd (bitwiseAnd (shiftRightZfBy myPawns 7) filesBH) theirPieces
    crToIdxs = Board.bbToIdxs crToSquares
    crMoves =
        List.keepIf crToIdxs isNormalRank
        |> List.map \toIdx ->
            captured = Board.pieceAt board (Square.idxToId toIdx)
            Move.createCapture (toIdx + 7) toIdx Piece.pawn captured

    # Capture right and promote
    crpMoves = createCapturePromotionMoves board crToIdxs (\toIdx -> toIdx + 7)

    # Capture left
    clToSquares = bitwiseAnd (bitwiseAnd (shiftRightZfBy myPawns 9) filesAG) theirPieces
    clToIdxs = Board.bbToIdxs clToSquares
    clMoves =
        List.keepIf clToIdxs isNormalRank
        |> List.map \toIdx ->
            captured = Board.pieceAt board (Square.idxToId toIdx)
            Move.createCapture (toIdx + 9) toIdx Piece.pawn captured

    # Capture left and promote
    clpMoves = createCapturePromotionMoves board clToIdxs (\toIdx -> toIdx + 9)

    # 'En passant' captures
    epMoves = if Board.isEnPassantAllowed board then createBlackEnPassantMoves board myPawns else []

    fmMoves
    |> List.concat nmMoves
    |> List.concat prMoves
    |> List.concat crMoves
    |> List.concat crpMoves
    |> List.concat clMoves
    |> List.concat clpMoves
    |> List.concat epMoves

# After 1. e4
expect
    board = Util.withMoves initialBoard ["e2e4"] White
    moves = generateBlackMoves board initialBoard.white |> toStr
    Set.fromList moves == Set.fromList ["a7a5", "b7b5", "c7c5", "d7d5", "e7e5", "f7f5", "g7g5", "h7h5", "a7a6", "b7b6", "c7c6", "d7d6", "e7e6", "f7f6", "g7g6", "h7h6"]
# After 1. e4 d5 2. Nf3
expect
    board = Util.withMoves initialBoard ["e2e4", "d7d5", "g1f3"] White
    moves = generateBlackMoves board board.white |> toStr
    Set.fromList moves == Set.fromList ["a7a5", "b7b5", "c7c5", "e7e5", "f7f5", "g7g5", "h7h5", "a7a6", "b7b6", "c7c6", "e7e6", "f7f6", "g7g6", "h7h6", "d5d4", "d5e4"]
# After 1. e4 e5
expect
    board = Util.withMoves initialBoard ["e2e4", "e7e5"] White
    # When calculating mobility score, we generate moves also for the side that just moved
    moves = generateBlackMoves board board.white |> toStr
    Set.fromList moves == Set.fromList ["a7a5", "b7b5", "c7c5", "d7d5", "f7f5", "g7g5", "h7h5", "a7a6", "b7b6", "c7c6", "d7d6", "f7f6", "g7g6", "h7h6"]

## Return true if the given square is on a normal rank, that is, not a promotion rank.
isNormalRank : SquareIdx -> Bool
isNormalRank = \toIdx ->
    toIdx > h1Idx && toIdx < a8Idx

## Create all possible promotion moves landing on the 'to square' indices in the list.
## The function f converts a 'to square' index to the corresponding 'from square' index.
createPromotionMoves : List SquareIdx, (SquareIdx -> SquareIdx) -> List Move
createPromotionMoves = \toIdxs, f ->
    List.dropIf toIdxs isNormalRank
    |> List.walk [] \list, toIdx ->
        List.concat list (List.map promotionPieces \promoted -> Move.createPromotion (f toIdx) toIdx promoted)

createCapturePromotionMoves : Board, List SquareIdx, (SquareIdx -> SquareIdx) -> List Move
createCapturePromotionMoves = \board, toIdxs, f ->
    List.dropIf toIdxs isNormalRank
    |> List.walk [] \list, toIdx ->
        captured = Board.pieceAt board (Square.idxToId toIdx)
        List.concat list (List.map promotionPieces \promoted -> Move.createCapturePromotion (f toIdx) toIdx captured promoted)

createWhiteEnPassantMoves : Board, Bitboard -> List Move
createWhiteEnPassantMoves = \board, myPawns ->
    enPassantIdx = Board.enPassantSquare board
    pawnIdxs = Board.bbToIdxs myPawns
    # Pawn must start on fifth row to make an 'en passant' capture
    List.keepIf pawnIdxs (\idx -> (idx // 8 == 4) && (enPassantIdx == idx + 7 || enPassantIdx == idx + 9))
    |> List.map \fromIdx ->
        Move.createEnPassant fromIdx enPassantIdx

createBlackEnPassantMoves : Board, Bitboard -> List Move
createBlackEnPassantMoves = \board, myPawns ->
    enPassantIdx = Board.enPassantSquare board
    pawnIdxs = Board.bbToIdxs myPawns
    # Pawn must start on fourth row to make an 'en passant' capture
    # Add to enPassantIdx instead of subtract from idx to avoid U64 underflow in case idx == 8 (pawn on square a2)
    # The && operator is not a short-circuit operator in Roc
    List.keepIf pawnIdxs (\idx -> (idx // 8 == 3) && (enPassantIdx + 7 == idx || enPassantIdx + 9 == idx))
    |> List.map \fromIdx ->
        Move.createEnPassant fromIdx enPassantIdx

expect
    board = Util.withMoves initialBoard ["e2e4"] White
    moves = createBlackEnPassantMoves board Square.d4 |> toStr
    Set.fromList moves == Set.fromList ["d4e3"]
expect
    board = Util.withMoves initialBoard ["e2e4", "e7e5"] White
    moves = createBlackEnPassantMoves board (bitwiseAnd board.pawn board.black) |> toStr
    Set.fromList moves == Set.fromList []
expect
    board = FenParser.fenToBoard Fen.crashInGenerateBlackEp
    moves = createBlackEnPassantMoves board (L.or [a2, a7, b7]) |> toStr
    Set.fromList moves == Set.fromList []

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

toStr : List Move -> List Str
toStr = \moves ->
    List.map moves \move -> Move.toStr move
