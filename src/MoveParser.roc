module [isMove, parse]

import Board exposing [Board, initialBoard]
import Checker
import Color exposing [Color]
import Move exposing [Move]
import MoveGenerator
import Num exposing [bitwiseXor]
import Piece exposing [PieceIdx]
import S
import Square exposing [SquareIdx, a2, a7, a8, b1Idx, c1Idx, c8Idx, e1Idx, e8Idx, f5Idx, g1Idx, g8Idx]
import Util

isMove : Str -> Bool
isMove = \str ->
    if S.len str == 4 || S.len str == 5 then
        Result.isOk (parseFrom str) && Result.isOk (parseTo str)
    else
        Bool.false

parse : Board, Color, Str -> Result Move [SyntaxError, IllegalMove]
parse = \board, sideToMove, str ->
    if S.len str == 4 || S.len str == 5 then
        from = parseFrom str
        to = parseTo str
        promoted = parsePromoted str
        captured = Result.map to \t -> Board.pieceAt board (Square.idxToId t)
        capturedColor = Result.try to \t -> Board.colorAt board (Square.idxToId t)

        if Result.isOk from && Result.isOk to then
            fromIdx = Result.withDefault from 0
            toIdx = Result.withDefault to 0
            movedIdx = Board.pieceAt board (Square.idxToId fromIdx)
            movedColor = Board.colorAt board (Square.idxToId fromIdx)
            if
                (fromIdx == toIdx)
                || (movedIdx == Piece.none)
                || (movedColor == Err SquareIsEmpty)
                || (movedColor != Ok sideToMove)
                || (capturedColor == Ok sideToMove)
                || (captured == Ok Piece.king)
                || (promoted == Ok Piece.king)
                || (promoted == Ok Piece.pawn)
                || (movedIdx == Piece.pawn && movedColor == Ok White && toIdx < fromIdx)
                || (movedIdx == Piece.pawn && movedColor == Ok Black && toIdx > fromIdx)
            then
                Err IllegalMove
            else
                createAndTestMove board sideToMove fromIdx toIdx movedIdx captured promoted
        else
            Err SyntaxError
    else
        Err SyntaxError

# Ok: Pawn move
expect parse initialBoard White "e2e4" == Ok Move.e2e4
# Ok: Knight move
expect parse initialBoard White "g1f3" == Ok Move.ng1f3
# Ok: Promotion
expect
    b = { initialBoard &
        # Move the White pawn from a2 to a7, and remove the Black pieces from a7 and a8
        white: bitwiseXor (bitwiseXor initialBoard.white a2) a7,
        black: bitwiseXor (bitwiseXor initialBoard.black a7) a8,
        pawn: bitwiseXor initialBoard.pawn a2,
        rook: bitwiseXor initialBoard.rook a8,
    }
    parse b White "a7a8q" == Ok Move.a7a8q
# Err: Promote to king
expect
    b = { initialBoard &
        # Move the White pawn from a2 to a7, and remove the Black pieces from a7 and a8
        white: bitwiseXor (bitwiseXor initialBoard.white a2) a7,
        black: bitwiseXor (bitwiseXor initialBoard.black a7) a8,
        pawn: bitwiseXor initialBoard.pawn a2,
        rook: bitwiseXor initialBoard.rook a8,
    }
    parse b White "a7a8k" == Err IllegalMove
# Err: Invalid length
expect parse initialBoard White "a7a" == Err SyntaxError
# Err: Invalid contents
expect parse initialBoard White "k2k4" == Err SyntaxError
# Err: No piece on 'from' square
expect parse initialBoard White "e4e5" == Err IllegalMove
# Ok: Black move
expect
    board = Util.withMoves initialBoard ["e2e4"] White
    parse board Black "d7d5" == Ok Move.d7d5
# Ok: White captures Black
expect
    board = Util.withMoves initialBoard ["e2e4", "d7d5"] White
    parse board White "e4d5" == Ok Move.e4d5
# Err: White captures White
expect parse initialBoard White "a1b1" == Err IllegalMove
# Err: Castling with blocking pieces
expect parse initialBoard White "e1g1" == Err IllegalMove
# Ok: White castles
expect
    board = Util.withMoves initialBoard ["e2e4", "e7e5", "g1f3", "g8f6", "f1c4", "f8c5"] White
    parse board White "e1g1" == Ok Move.e1g1
# Err: White is in check
expect
    board = Util.withMoves initialBoard ["f2f3", "e7e5", "g2g4", "d8h4"] White
    parse board White "e2e4" == Err IllegalMove
# Err: From and to squares are the same
expect parse initialBoard White "a1a1" == Err IllegalMove
# Err: White pawn moves backwards
expect
    board = Util.withMoves initialBoard ["e2e4", "e7e5"] White
    parse board White "e4e2" == Err IllegalMove
# Err: Black pawn moves backwards
expect
    board = Util.withMoves initialBoard ["e2e4", "e7e5", "d2d4"] White
    parse board Black "e5e7" == Err IllegalMove

createAndTestMove :
    Board,
    Color,
    SquareIdx,
    SquareIdx,
    PieceIdx,
    Result PieceIdx _,
    Result PieceIdx _
    ->
    Result Move _
createAndTestMove = \board, sideToMove, fromIdx, toIdx, movedIdx, captured, promoted ->
    optionalMove =
        if isEnPassant board toIdx movedIdx then
            Ok (Move.createEnPassant fromIdx toIdx)
        else if isCastling fromIdx toIdx movedIdx then
            castle board fromIdx toIdx
        else if isCapture captured && isPromotion promoted then
            captureAndPromote fromIdx toIdx captured promoted
        else if isCapture captured then
            capture fromIdx toIdx movedIdx captured
        else if isPromotion promoted then
            promote fromIdx toIdx promoted
        else
            # Normal move!
            Ok (Move.create fromIdx toIdx movedIdx)
    when optionalMove is
        Ok move -> testMove board sideToMove move
        Err error -> Err error

testMove : Board, Color, Move -> Result Move [IllegalMove]
testMove = \board, sideToMove, move ->
    boardAfterMove = Board.makeMove board move sideToMove
    boardWithMoves = MoveGenerator.withMoves boardAfterMove
    if Checker.isCheck boardWithMoves sideToMove then
        Err IllegalMove
    else
        Ok move

isPromotion : Result PieceIdx _ -> Bool
isPromotion = \promoted ->
    when promoted is
        Ok pieceIdx -> pieceIdx != Piece.none
        Err _ -> Bool.false

isCapture : Result PieceIdx _ -> Bool
isCapture = \captured ->
    when captured is
        Ok pieceIdx -> pieceIdx != Piece.none
        Err _ -> Bool.false

expect isCapture (Ok Piece.bishop)
expect isCapture (Ok Piece.pawn)
expect isCapture (Ok Piece.none) == Bool.false
expect isCapture (Err SyntaxError) == Bool.false

capture = \fromIdx, toIdx, movedIdx, captured ->
    capturedIdx = Result.withDefault captured Piece.none
    Ok (Move.createCapture fromIdx toIdx movedIdx capturedIdx)

captureAndPromote = \fromIdx, toIdx, captured, promoted ->
    promotedIdx = Result.withDefault promoted 0
    capturedIdx = Result.withDefault captured Piece.none
    Ok (Move.createCapturePromotion fromIdx toIdx capturedIdx promotedIdx)

promote = \fromIdx, toIdx, promoted ->
    promotedIdx = Result.withDefault promoted 0
    Ok (Move.createPromotion fromIdx toIdx promotedIdx)

isCastling : SquareIdx, SquareIdx, PieceIdx -> Bool
isCastling = \fromIdx, toIdx, movedIdx ->
    (movedIdx == Piece.king)
    &&
    ((fromIdx == e1Idx && toIdx == c1Idx) || (fromIdx == e1Idx && toIdx == g1Idx) || (fromIdx == e8Idx && toIdx == c8Idx) || (fromIdx == e8Idx && toIdx == g8Idx))

expect isCastling e1Idx c1Idx Piece.king
expect isCastling e1Idx g1Idx Piece.king
expect isCastling e8Idx c8Idx Piece.king
expect isCastling e8Idx g8Idx Piece.king
expect isCastling f5Idx g8Idx Piece.king == Bool.false
expect isCastling e1Idx b1Idx Piece.king == Bool.false
expect isCastling e1Idx c1Idx Piece.rook == Bool.false

isEnPassant : Board, SquareIdx, PieceIdx -> Bool
isEnPassant = \board, toIdx, movedIdx ->
    (movedIdx == Piece.pawn)
    && (Board.isEnPassantAllowed board)
    && (Board.enPassantSquare board == toIdx)

castle : Board, SquareIdx, SquareIdx -> Result Move [IllegalMove]
castle = \board, fromIdx, toIdx ->
    # We already checked that this is a castling move, now we only check that it is legal
    if Board.isCastlingAllowed board fromIdx toIdx then
        Ok (Move.createCastling fromIdx toIdx)
    else
        Err IllegalMove

parseFrom : Str -> Result SquareIdx [BadUtf8, SyntaxError]
parseFrom = \str ->
    S.substr str { start: 0, len: 2 } |> Result.try Square.fromStr

parseTo : Str -> Result SquareIdx [BadUtf8, SyntaxError]
parseTo = \str ->
    S.substr str { start: 2, len: 2 } |> Result.try Square.fromStr

parsePromoted : Str -> Result PieceIdx [BadUtf8, SyntaxError]
parsePromoted = \str ->
    if S.len str == 5 then
        S.substr str { start: 4, len: 1 } |> Result.try Piece.fromStr
    else
        Err SyntaxError
