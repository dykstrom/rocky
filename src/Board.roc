module [Bitboard, Board, emptyBoard, initialBoard, makeMove, colorAt, pieceAt, isLegal, isCastlingAllowed, withCastlingRights, isEnPassantAllowed, enPassantSquare, withEnPassantSquare, halfMoveClock, withHalfMoveClock, equalsPieces, toStr, toPrettyStr, withMoves, bbToStr, bbToIdxs]

import Ansi
import Color exposing [Color]
import L
import Move exposing [Move, e1g1, e2e4, e7e5, f1c4, f3g1, f6g8, f8c5, g7g5, h2h4, h4h5, h5g6, ng1f3, ng8f6]
import Num exposing [bitwiseAnd, bitwiseNot, bitwiseOr, bitwiseXor, shiftLeftBy, shiftRightZfBy]
import Piece exposing [PieceIdx]
import S
import Square exposing [SquareId, SquareIdx, a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2, e4, g5, g6, a7, b7, c7, d7, e7, f7, g7, h7, a8, b8, c8, d8, e8, f8, g8, h8, a1Idx, b1Idx, b8Idx, c1Idx, c8Idx, d1Idx, d5Idx, d6Idx, d7Idx, e1Idx, e3Idx, e8Idx, f1Idx, g1Idx, g6Idx, g8Idx, h1Idx, a2Idx, b2Idx, c2Idx, d2Idx, e2Idx, f2Idx, g2Idx, h2Idx]

Bitboard : U64

Board : {
    white : Bitboard,
    black : Bitboard,
    bishop : Bitboard,
    king : Bitboard,
    knight : Bitboard,
    queen : Bitboard,
    pawn : Bitboard,
    rook : Bitboard,
    flags : U64,
    whiteMoves : List Move,
    blackMoves : List Move,
}

squareMask = 0x3fu64

# The field flags is a bitset of flags that define the following data:
# - Bit 00-06 - half-move clock
# - Bit 07-08 - castling rights for white (1 = yes, 0 = no)
# - Bit 09-0a - castling rights for black (1 = yes, 0 = no)
# - Bit 0b-10 - en passant square
halfMoveMask = 0x7fu64
wksCastlingMask = shiftLeftBy 1u64 0x07u8
wqsCastlingMask = shiftLeftBy 1u64 0x08u8
bksCastlingMask = shiftLeftBy 1u64 0x09u8
bqsCastlingMask = shiftLeftBy 1u64 0x0au8
enPassantOffset = 0x0bu8
enPassantMask = shiftLeftBy squareMask enPassantOffset

notHalfMoveMask = bitwiseNot halfMoveMask
notWhiteCastlingMask = bitwiseNot (bitwiseOr wksCastlingMask wqsCastlingMask)
notBlackCastlingMask = bitwiseNot (bitwiseOr bksCastlingMask bqsCastlingMask)
notEnPassantMask = bitwiseNot enPassantMask

emptyBoard = {
    white: 0,
    black: 0,
    bishop: 0,
    king: 0,
    knight: 0,
    queen: 0,
    pawn: 0,
    rook: 0,
    flags: 0,
    whiteMoves: [],
    blackMoves: [],
}

initialBoard = {
    white: L.or [a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2],
    black: L.or [a7, b7, c7, d7, e7, f7, g7, h7, a8, b8, c8, d8, e8, f8, g8, h8],
    bishop: L.or [c1, f1, c8, f8],
    king: L.or [e1, e8],
    knight: L.or [b1, g1, b8, g8],
    queen: L.or [d1, d8],
    pawn: L.or [a2, b2, c2, d2, e2, f2, g2, h2, a7, b7, c7, d7, e7, f7, g7, h7],
    rook: L.or [a1, h1, a8, h8],
    flags: L.or [wksCastlingMask, wqsCastlingMask, bksCastlingMask, bqsCastlingMask],
    whiteMoves: [],
    blackMoves: [],
}

expect bitwiseAnd initialBoard.rook a1 != 0
expect bitwiseAnd initialBoard.rook h1 != 0
expect bitwiseAnd initialBoard.rook a8 != 0
expect bitwiseAnd initialBoard.rook h8 != 0

# Squares that must be empty when castling
b1c1d1 = L.or [b1, c1, d1]
f1g1 = L.or [f1, g1]
b8c8d8 = L.or [b8, c8, d8]
f8g8 = L.or [f8, g8]

colorAt : Board, SquareId -> Result Color [SquareIsEmpty]
colorAt = \board, square ->
    if bitwiseAnd board.white square != 0 then
        Ok White
    else if bitwiseAnd board.black square != 0 then
        Ok Black
    else
        Err SquareIsEmpty

expect colorAt initialBoard e1 == Ok White
expect colorAt initialBoard e8 == Ok Black
expect colorAt initialBoard e4 == Err SquareIsEmpty

pieceAt : Board, SquareId -> PieceIdx
pieceAt = \board, square ->
    if bitwiseAnd board.bishop square != 0 then
        Piece.bishop
    else if bitwiseAnd board.king square != 0 then
        Piece.king
    else if bitwiseAnd board.knight square != 0 then
        Piece.knight
    else if bitwiseAnd board.pawn square != 0 then
        Piece.pawn
    else if bitwiseAnd board.queen square != 0 then
        Piece.queen
    else if bitwiseAnd board.rook square != 0 then
        Piece.rook
    else
        Piece.none

expect pieceAt initialBoard e1 == Piece.king
expect pieceAt initialBoard b8 == Piece.knight
expect pieceAt initialBoard h8 == Piece.rook
expect pieceAt initialBoard e4 == Piece.none

isLegal : Board -> Bool
isLegal = \board ->
    (List.len (bbToIdxs (bitwiseAnd board.white board.king)) == 1)
    &&
    (List.len (bbToIdxs (bitwiseAnd board.black board.king)) == 1)

expect isLegal initialBoard == Bool.true
expect isLegal emptyBoard == Bool.false

isCastlingAllowed : Board, SquareIdx, SquareIdx -> Bool
isCastlingAllowed = \board, fromIdx, toIdx ->
    if fromIdx == e1Idx && toIdx == c1Idx then
        ica board e1 a1 b1c1d1 wqsCastlingMask
    else if fromIdx == e1Idx && toIdx == g1Idx then
        ica board e1 h1 f1g1 wksCastlingMask
    else if fromIdx == e8Idx && toIdx == c8Idx then
        ica board e8 a8 b8c8d8 bqsCastlingMask
    else if fromIdx == e8Idx && toIdx == g8Idx then
        ica board e8 h8 f8g8 bksCastlingMask
    else
        Bool.false

expect isCastlingAllowed initialBoard e1Idx c1Idx == Bool.false
expect isCastlingAllowed initialBoard e1Idx g1Idx == Bool.false
expect isCastlingAllowed initialBoard e8Idx c8Idx == Bool.false
expect isCastlingAllowed initialBoard e8Idx g8Idx == Bool.false

ica : Board, SquareId, SquareId, Bitboard, U64 -> Bool
ica = \board, kingSquare, rookSquare, blockingSquares, castlingMask ->
    (pieceAt board rookSquare == Piece.rook)
    && (colorAt board kingSquare == colorAt board rookSquare)
    && ((bitwiseAnd (bitwiseOr board.white board.black) blockingSquares) == 0)
    && (bitwiseAnd board.flags castlingMask != 0)

isEnPassantAllowed : Board -> Bool
isEnPassantAllowed = \board ->
    # This works because the en passant square is always on the 3rd or 6th rank.
    # It can never be a1, which would return false here.
    bitwiseAnd board.flags enPassantMask != 0

expect isEnPassantAllowed initialBoard == Bool.false
expect
    board = { initialBoard &
        flags: bitwiseOr initialBoard.flags (shiftLeftBy g6Idx enPassantOffset),
    }
    isEnPassantAllowed board

enPassantSquare : Board -> SquareIdx
enPassantSquare = \board ->
    shiftRightZfBy (bitwiseAnd board.flags enPassantMask) enPassantOffset

expect
    board = { initialBoard & flags: bitwiseOr initialBoard.flags (shiftLeftBy g6Idx enPassantOffset) }
    enPassantSquare board == g6Idx

halfMoveClock : Board -> U64
halfMoveClock = \board ->
    bitwiseAnd board.flags halfMoveMask

expect halfMoveClock initialBoard == 0
expect
    board = { initialBoard & flags: initialBoard.flags + 53 }
    halfMoveClock board == 53

## Return true if the two boards are equal if only the pieces are considered.
equalsPieces : Board, Board -> Bool
equalsPieces = \board1, board2 ->
    (board1.white == board2.white)
    && (board1.black == board2.black)
    && (board1.bishop == board2.bishop)
    && (board1.king == board2.king)
    && (board1.knight == board2.knight)
    && (board1.pawn == board2.pawn)
    && (board1.queen == board2.queen)
    && (board1.rook == board2.rook)

expect
    board = withMoves initialBoard [ng1f3, ng8f6, f3g1, f6g8] White
    (board != initialBoard)
    &&
    (equalsPieces board initialBoard == Bool.true)

toStr : Board -> Str
toStr = \board ->
    ranks = List.range { start: At 7, end: At 0 }
    files = List.range { start: At 0, end: At 7 }

    List.map ranks \r ->
        List.map files \f ->
            square = Square.frToId f r
            color = colorAt board square
            piece = pieceAt board square
            pieceStr = if piece == Piece.none then "." else Piece.toStr piece
            if color == Ok White then
                Result.withDefault (S.toUpper pieceStr) "?"
            else
                pieceStr
        |> Str.joinWith ""
        |> \line -> Str.concat (Num.toStr (r + 1)) " | " |> Str.concat line |> Str.concat " |"
    |> Str.joinWith "\n"
    |> \s -> Str.concat "   ---------- \n" s
    |> Str.concat "\n   ---------- \n    abcdefgh "

toPrettyStr : Board -> Str
toPrettyStr = \board ->
    ranks = List.range { start: At 7, end: At 0 }
    files = List.range { start: At 0, end: At 7 }

    List.map ranks \r ->
        List.map files \f ->
            square = Square.frToId f r
            color = colorAt board square
            piece = pieceAt board square
            character =
                when color is
                    Ok White -> Piece.toPrettyStr piece White
                    Ok Black -> Piece.toPrettyStr piece Black
                    _ -> " "
            Str.concat (backgroundFor f r) character |> Str.concat Ansi.default
        |> Str.joinWith ""
        |> \line -> Str.concat (Num.toStr (r + 1)) " │ " |> Str.concat line |> Str.concat " │"
    |> Str.joinWith "\n"
    |> \s -> Str.concat "  ┌──────────┐\n" s
    |> Str.concat "\n  └──────────┘\n    abcdefgh "

backgroundFor : U8, U8 -> Str
backgroundFor = \file, rank ->
    if file % 2 == rank % 2 then Ansi.dark else Ansi.light

makeMove : Board, Move, Color -> Board
makeMove = \board, move, color ->
    fromId = Square.idxToId (Move.getFrom move)
    toId = Square.idxToId (Move.getTo move)
    moved = Move.getMoved move
    expect moved == pieceAt board fromId
    captured = Move.getCaptured move
    expect captured == pieceAt board toId || Move.isEnPassant move
    promoted = Move.getPromoted move

    afterMove =
        if Move.isCastling move then
            castle board fromId toId color
        else if Move.isEnPassant move then
            moveFrom board fromId moved color
            |> moveTo toId moved color
            |> captureEnPassant toId color
        else
            moveFrom board fromId moved color
            |> moveTo toId moved color
            |> \b -> if captured != Piece.none then capturePiece b toId captured color else b
            |> \b -> if promoted != Piece.none then promotePiece b toId promoted else b
    afterMove
    |> updateCastlingFlags fromId
    |> updateEnPassantSquare (Move.getFrom move) (Move.getTo move) moved
    |> updateHalfMoveClock moved (captured != Piece.none)

# Ok: White move
expect
    newBoard = makeMove initialBoard e2e4 White
    newBoard.white
    == bitwiseOr (bitwiseXor initialBoard.white e2) e4
    && newBoard.black
    == initialBoard.black
    && newBoard.bishop
    == initialBoard.bishop
    && newBoard.knight
    == initialBoard.knight
    && newBoard.king
    == initialBoard.king
    && newBoard.pawn
    == bitwiseOr (bitwiseXor initialBoard.pawn e2) e4
    && newBoard.queen
    == initialBoard.queen
    && newBoard.rook
    == initialBoard.rook
    && isEnPassantAllowed newBoard
    == Bool.true
    && enPassantSquare newBoard
    == e3Idx

# Ok: White castling
expect
    boardWhereWhiteCanCastle = withMoves initialBoard [e2e4, e7e5, ng1f3, ng8f6, f1c4, f8c5] White
    newBoard = makeMove boardWhereWhiteCanCastle e1g1 White

    pieceAt newBoard e1
    == Piece.none
    && pieceAt newBoard f1
    == Piece.rook
    && pieceAt newBoard g1
    == Piece.king
    && pieceAt newBoard h1
    == Piece.none
    && colorAt newBoard e1
    == Err SquareIsEmpty
    && colorAt newBoard f1
    == Ok White
    && colorAt newBoard g1
    == Ok White
    && colorAt newBoard h1
    == Err SquareIsEmpty
    && bitwiseAnd newBoard.flags wksCastlingMask
    == 0
    && bitwiseAnd newBoard.flags wqsCastlingMask
    == 0

# Ok: White captures 'en passant' on g6
expect
    board = withMoves initialBoard [h2h4, e7e5, h4h5, g7g5] White
    newBoard = makeMove board h5g6 White

    pieceAt newBoard g5
    == Piece.none
    && pieceAt newBoard g6
    == Piece.pawn
    && colorAt newBoard g5
    == Err SquareIsEmpty
    && colorAt newBoard g6
    == Ok White
    && isEnPassantAllowed newBoard
    == Bool.false
    && enPassantSquare newBoard
    == 0

updateCastlingFlags : Board, SquareId -> Board
updateCastlingFlags = \board, fromId ->
    if fromId == e1 then
        { board & flags: bitwiseAnd board.flags notWhiteCastlingMask }
    else if fromId == a1 then
        { board & flags: bitwiseAnd board.flags (bitwiseNot wqsCastlingMask) }
    else if fromId == h1 then
        { board & flags: bitwiseAnd board.flags (bitwiseNot wksCastlingMask) }
    else if fromId == e8 then
        { board & flags: bitwiseAnd board.flags notBlackCastlingMask }
    else if fromId == a8 then
        { board & flags: bitwiseAnd board.flags (bitwiseNot bqsCastlingMask) }
    else if fromId == h8 then
        { board & flags: bitwiseAnd board.flags (bitwiseNot bksCastlingMask) }
    else
        board

withCastlingRights : Board, Color, PieceIdx -> Board
withCastlingRights = \board, color, piece ->
    when (color, piece) is
        (White, 2) -> { board & flags: bitwiseOr board.flags wksCastlingMask }
        (White, 5) -> { board & flags: bitwiseOr board.flags wqsCastlingMask }
        (Black, 2) -> { board & flags: bitwiseOr board.flags bksCastlingMask }
        (Black, 5) -> { board & flags: bitwiseOr board.flags bqsCastlingMask }
        _ -> crash "Should not happen: unknown piece in withCastlingRights: $(Num.toStr piece)"

updateEnPassantSquare : Board, SquareIdx, SquareIdx, PieceIdx -> Board
updateEnPassantSquare = \board, fromIdx, toIdx, moved ->
    bitwiseAnd board.flags notEnPassantMask
    |> \flags ->
        if moved == Piece.pawn then
            if toIdx == fromIdx + 16 then
                bitwiseOr flags (shiftLeftBy (fromIdx + 8) enPassantOffset)
            else if fromIdx == toIdx + 16 then
                bitwiseOr flags (shiftLeftBy (toIdx + 8) enPassantOffset)
            else
                flags
        else
            flags
    |> \flags -> { board & flags }

expect
    board = updateEnPassantSquare initialBoard d7Idx d5Idx Piece.pawn
    enPassantSquare board == d6Idx && isEnPassantAllowed board
expect
    board = updateEnPassantSquare initialBoard d6Idx d5Idx Piece.pawn
    enPassantSquare board == 0 && isEnPassantAllowed board == Bool.false
expect
    board = updateEnPassantSquare initialBoard d7Idx d5Idx Piece.rook
    enPassantSquare board == 0 && isEnPassantAllowed board == Bool.false

withEnPassantSquare : Board, SquareIdx -> Board
withEnPassantSquare = \board, square ->
    { board & flags: bitwiseAnd board.flags notEnPassantMask |> bitwiseOr (shiftLeftBy square enPassantOffset) }

expect
    board = withEnPassantSquare initialBoard d5Idx
    (isEnPassantAllowed board == Bool.true)
    && (enPassantSquare board == d5Idx)

updateHalfMoveClock : Board, PieceIdx, Bool -> Board
updateHalfMoveClock = \board, moved, isCapture ->
    if isCapture || moved == Piece.pawn then
        { board & flags: bitwiseAnd board.flags notHalfMoveMask }
    else
        { board & flags: board.flags + 1 }

withHalfMoveClock : Board, U64 -> Board
withHalfMoveClock = \board, clock ->
    { board & flags: bitwiseAnd board.flags notHalfMoveMask |> bitwiseOr clock }

expect
    board = withHalfMoveClock initialBoard 17
    halfMoveClock board == 17

moveFrom : Board, SquareId, PieceIdx, Color -> Board
moveFrom = \board, square, moved, color ->
    (newWhite, newBlack) =
        if color == White then
            (bitwiseXor board.white square, board.black)
        else
            (board.white, bitwiseXor board.black square)

    (newBishop, newKing, newKnight, newPawn, newQueen, newRook) =
        when moved is
            1 -> (bitwiseXor board.bishop square, board.king, board.knight, board.pawn, board.queen, board.rook)
            2 -> (board.bishop, bitwiseXor board.king square, board.knight, board.pawn, board.queen, board.rook)
            3 -> (board.bishop, board.king, bitwiseXor board.knight square, board.pawn, board.queen, board.rook)
            4 -> (board.bishop, board.king, board.knight, bitwiseXor board.pawn square, board.queen, board.rook)
            5 -> (board.bishop, board.king, board.knight, board.pawn, bitwiseXor board.queen square, board.rook)
            6 -> (board.bishop, board.king, board.knight, board.pawn, board.queen, bitwiseXor board.rook square)
            _ -> crash "Should not happen: unknown piece in moveFrom: $(Num.toStr moved)"

    { board &
        white: newWhite,
        black: newBlack,
        bishop: newBishop,
        king: newKing,
        knight: newKnight,
        pawn: newPawn,
        queen: newQueen,
        rook: newRook,
    }

moveTo : Board, SquareId, PieceIdx, Color -> Board
moveTo = \board, square, moved, color ->
    (newWhite, newBlack) =
        if color == White then
            (bitwiseOr board.white square, board.black)
        else
            (board.white, bitwiseOr board.black square)

    (newBishop, newKing, newKnight, newPawn, newQueen, newRook) =
        when moved is
            1 -> (bitwiseXor board.bishop square, board.king, board.knight, board.pawn, board.queen, board.rook)
            2 -> (board.bishop, bitwiseXor board.king square, board.knight, board.pawn, board.queen, board.rook)
            3 -> (board.bishop, board.king, bitwiseXor board.knight square, board.pawn, board.queen, board.rook)
            4 -> (board.bishop, board.king, board.knight, bitwiseXor board.pawn square, board.queen, board.rook)
            5 -> (board.bishop, board.king, board.knight, board.pawn, bitwiseXor board.queen square, board.rook)
            6 -> (board.bishop, board.king, board.knight, board.pawn, board.queen, bitwiseXor board.rook square)
            _ -> crash "Should not happen: unknown piece in moveTo: $(Num.toStr moved)"

    { board &
        white: newWhite,
        black: newBlack,
        bishop: newBishop,
        king: newKing,
        knight: newKnight,
        pawn: newPawn,
        queen: newQueen,
        rook: newRook,
    }

promotePiece : Board, SquareId, PieceIdx -> Board
promotePiece = \board, square, promoted ->
    { board &
        bishop: if promoted == Piece.bishop then bitwiseOr board.bishop square else board.bishop,
        knight: if promoted == Piece.knight then bitwiseOr board.knight square else board.knight,
        queen: if promoted == Piece.queen then bitwiseOr board.queen square else board.queen,
        rook: if promoted == Piece.rook then bitwiseOr board.rook square else board.rook,
        pawn: bitwiseXor board.pawn square,
    }

## Return the board with the captured piece removed.
capturePiece : Board, SquareId, PieceIdx, Color -> Board
capturePiece = \board, square, captured, color ->
    { board &
        bishop: if captured == Piece.bishop then bitwiseXor board.bishop square else board.bishop,
        king: if captured == Piece.king then bitwiseXor board.king square else board.king,
        knight: if captured == Piece.knight then bitwiseXor board.knight square else board.knight,
        pawn: if captured == Piece.pawn then bitwiseXor board.pawn square else board.pawn,
        queen: if captured == Piece.queen then bitwiseXor board.queen square else board.queen,
        rook: if captured == Piece.rook then bitwiseXor board.rook square else board.rook,
        white: if color == White then board.white else bitwiseXor board.white square,
        black: if color == White then bitwiseXor board.black square else board.black,
    }

## Return the board after castling with the king moving from kf to kt.
castle : Board, SquareId, SquareId, Color -> Board
castle = \board, kf, kt, color ->
    (rf, rt) =
        if kt == c1 then
            (a1, d1)
        else if kt == g1 then
            (h1, f1)
        else if kt == c8 then
            (a8, d8)
        else if kt == g8 then
            (h8, f8)
        else
            crash "Should not happen: invalid from square: $(Square.toStr kt)"

    { board &
        white: if color == White then
            bitwiseXor (bitwiseXor (bitwiseXor (bitwiseXor board.white kf) kt) rf) rt
        else
            board.white,
        black: if color == Black then
            bitwiseXor (bitwiseXor (bitwiseXor (bitwiseXor board.black kf) kt) rf) rt
        else
            board.black,
        king: bitwiseXor (bitwiseXor board.king kf) kt,
        rook: bitwiseXor (bitwiseXor board.rook rf) rt,
    }

## Return the board with the piece captured 'en passant' removed.
captureEnPassant : Board, SquareId, Color -> Board
captureEnPassant = \board, toId, sideToMove ->
    capturedSquareId =
        if sideToMove == White then
            shiftRightZfBy toId 8
        else
            shiftLeftBy toId 8
    { board &
        pawn: bitwiseXor board.pawn capturedSquareId,
        white: if sideToMove == White then board.white else bitwiseXor board.white capturedSquareId,
        black: if sideToMove == White then bitwiseXor board.black capturedSquareId else board.black,
    }

## Setup the given board by making the given moves.
withMoves : Board, List Move, Color -> Board
withMoves = \board, moves, sideToMove ->
    List.walk moves { b: board, c: sideToMove } \state, move ->
        newBoard = makeMove state.b move state.c
        newColor = Color.flipColor state.c
        { b: newBoard, c: newColor }
    |> \state -> state.b

expect withMoves initialBoard [ng1f3, ng8f6, f3g1, f6g8] White == { initialBoard & flags: initialBoard.flags + 4 }

## Convert the given bitboard to a string of 0s and 1s.
bbToStr : Bitboard -> Str
bbToStr = \bitboard ->
    ranks = List.range { start: At 7, end: At 0 }
    files = List.range { start: At 0, end: At 7 }

    List.map ranks \r ->
        List.map files \f ->
            square = Square.frToId f r
            if bitwiseAnd bitboard square != 0 then "1" else "0"
        |> Str.joinWith ""
    |> Str.joinWith "\n"
    |> \s -> Str.concat (Str.concat "\n" s) "\n"

expect bbToStr initialBoard.white == "\n00000000\n00000000\n00000000\n00000000\n00000000\n00000000\n11111111\n11111111\n"

## Return a list of square indices: one index for each square that is occupied in the given bitboard.
bbToIdxs : Bitboard -> List SquareIdx
bbToIdxs = \bitboard ->
    iter = \b, list ->
        if b == 0 then
            list
        else
            idx = Num.countTrailingZeroBits b
            next = bitwiseAnd b (bitwiseNot (shiftLeftBy 1u64 idx))
            iter next (List.append list (Num.toU64 idx))
    iter bitboard (List.withCapacity 64)

expect bbToIdxs 0 == []
expect bbToIdxs initialBoard.knight == [b1Idx, g1Idx, b8Idx, g8Idx]
expect bbToIdxs initialBoard.king == [e1Idx, e8Idx]
expect bbToIdxs initialBoard.white == [a1Idx, b1Idx, c1Idx, d1Idx, e1Idx, f1Idx, g1Idx, h1Idx, a2Idx, b2Idx, c2Idx, d2Idx, e2Idx, f2Idx, g2Idx, h2Idx]
