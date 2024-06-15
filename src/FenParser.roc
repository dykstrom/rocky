module [fromList, fenToBoard]

import Board exposing [Board, emptyBoard, initialBoard]
import Color exposing [Color]
import Fen
import Game exposing [Game, initialGame]
import Num exposing [bitwiseOr]
import Piece exposing [PieceIdx]
import Square exposing [e4]
import Util

## Parse a FEN string and return a Game initialized from that string.
fromStr : Str -> Result Game [IllegalPosition, SyntaxError]
fromStr = \fen ->
    fromList (Str.split fen " ")

fromList : List Str -> Result Game [IllegalPosition, SyntaxError]
fromList = \fen ->
    result =
        when fen is
            [pieces, activeColor, castlingRights, enPassant, halfMoveClock, fullMoveNumber] ->
                Ok { initialGame & board: emptyBoard }
                |> Result.try \g -> withPieces g pieces
                |> Result.try \g -> withActiveColor g activeColor
                |> Result.try \g -> withCastlingRights g castlingRights
                |> Result.try \g -> withEnPassant g enPassant
                |> Result.try \g -> withMoveNumber g fullMoveNumber
                |> Result.try \g -> withHalfMoveClock g halfMoveClock

            _ -> Err SyntaxError
    # Check that parsing the FEN string resulted in a legal position
    result
    |> Result.try \g -> if Board.isLegal g.board then Ok g else Err SyntaxError
    |> Result.mapErr \_ -> SyntaxError

expect fromStr Fen.initialGame == Ok initialGame
expect
    boardFromFen =
        fromStr Fen.foolsMate
        |> Result.map .board
        |> Result.withDefault initialBoard
    boardFromMoves = Util.withMoves initialBoard ["f2f3", "e7e5", "g2g4", "d8h4"] White
    boardFromFen == boardFromMoves
expect
    boardFromFen =
        fromStr Fen.scholarsMate
        |> Result.map .board
        |> Result.withDefault initialBoard
    boardFromMoves = Util.withMoves initialBoard ["e2e4", "e7e5", "f1c4", "f8c5", "d1h5", "g8f6", "h5f7"] White
    boardFromFen == boardFromMoves
expect fromStr Fen.syntaxError == Err SyntaxError

withPieces = \game, str ->
    ranks = Str.split str "/"
    if List.len ranks == 8 then
        withRanks game.board ranks |> Result.map \b -> { game & board: b }
    else
        Err SyntaxError

withRanks : Board, List Str -> Result Board [SyntaxError]
withRanks = \board, ranks ->
    List.walkTry ranks { b: board, r: 0 } \state, str ->
        when withRank state.b state.r str is
            Ok b -> Ok { b: b, r: state.r + 1 }
            Err SyntaxError -> Err SyntaxError
    |> Result.map .b

withRank : Board, U8, Str -> Result Board [SyntaxError]
withRank = \board, rank, str ->
    List.walkTry (Str.toUtf8 str) { b: board, r: rank, f: 0 } iter
    |> Result.map .b

iter : { b : Board, r : U8, f : U8 }, U8 -> Result { b : Board, r : U8, f : U8 } [SyntaxError]
iter = \state, byte ->
    when byte is
        66 -> Ok (withPiece state Piece.bishop White)
        75 -> Ok (withPiece state Piece.king White)
        78 -> Ok (withPiece state Piece.knight White)
        80 -> Ok (withPiece state Piece.pawn White)
        81 -> Ok (withPiece state Piece.queen White)
        82 -> Ok (withPiece state Piece.rook White)
        98 -> Ok (withPiece state Piece.bishop Black)
        107 -> Ok (withPiece state Piece.king Black)
        110 -> Ok (withPiece state Piece.knight Black)
        112 -> Ok (withPiece state Piece.pawn Black)
        113 -> Ok (withPiece state Piece.queen Black)
        114 -> Ok (withPiece state Piece.rook Black)
        # Number of empty squares
        n if n >= 48 && n <= 56 -> Ok { state & f: state.f + n - 48 }
        _ -> Err SyntaxError

expect
    # In FEN strings, the rank numbers are reversed, hence rank 4 below
    when iter { b: initialBoard, r: 4, f: 4 } 66 is
        Ok state ->
            (state.r == 4)
            && (state.f == 5)
            && (Board.pieceAt state.b e4 == Piece.bishop)
            && (Board.colorAt state.b e4 == Ok White)

        Err _ -> 1 == 0

expect
    result = iter { b: initialBoard, r: 4, f: 4 } 17
    result == Err SyntaxError

withPiece : { b : Board, r : U8, f : U8 }, PieceIdx, Color -> { b : Board, r : U8, f : U8 }
withPiece = \state, piece, color ->
    board = state.b
    squareId = Square.frToId state.f (7 - state.r)
    boardWithPiece =
        when piece is
            1 -> { board & bishop: bitwiseOr board.bishop squareId }
            2 -> { board & king: bitwiseOr board.king squareId }
            3 -> { board & knight: bitwiseOr board.knight squareId }
            4 -> { board & pawn: bitwiseOr board.pawn squareId }
            5 -> { board & queen: bitwiseOr board.queen squareId }
            6 -> { board & rook: bitwiseOr board.rook squareId }
            _ -> crash "Should not happen: unknown piece in withPiece: $(Num.toStr piece)"
    boardWithColor =
        when color is
            White -> { boardWithPiece & white: bitwiseOr board.white squareId }
            Black -> { boardWithPiece & black: bitwiseOr board.black squareId }
    { state & b: boardWithColor, f: state.f + 1 }

withActiveColor = \game, str ->
    when Str.trim str is
        "w" -> Ok { game & activeColor: White }
        "b" -> Ok { game & activeColor: Black }
        _ -> Err SyntaxError

expect
    game = withActiveColor initialGame "b"
    game == Ok { initialGame & activeColor: Black }
expect withActiveColor initialGame "x" == Err SyntaxError

withCastlingRights = \game, str ->
    game
    |> \g -> if Str.contains str "K" then { g & board: Board.withCastlingRights g.board White Piece.king } else g
    |> \g -> if Str.contains str "Q" then { g & board: Board.withCastlingRights g.board White Piece.queen } else g
    |> \g -> if Str.contains str "k" then { g & board: Board.withCastlingRights g.board Black Piece.king } else g
    |> \g -> if Str.contains str "q" then { g & board: Board.withCastlingRights g.board Black Piece.queen } else g
    |> Ok

withMoveNumber = \game, str ->
    when Str.trim str |> Str.toU64 is
        Ok number -> Ok { game & moveNumber: number }
        _ -> Err SyntaxError

expect
    game = withMoveNumber initialGame "82"
    game == Ok { initialGame & moveNumber: 82 }

withHalfMoveClock = \game, str ->
    when Str.trim str |> Str.toU64 is
        Ok number -> Ok { game & board: Board.withHalfMoveClock game.board number }
        _ -> Err SyntaxError

withEnPassant = \game, str ->
    Str.trim str
    |> \trimmed -> if trimmed == "-" then Ok 0 else Square.fromStr trimmed
    |> Result.map \square -> { game & board: Board.withEnPassantSquare game.board square }

## Setup a board from the given FEN string.
fenToBoard : Str -> Board
fenToBoard = \fen ->
    when fromStr fen is
        Ok game -> game.board
        _ -> crash "Should not happen: invalid FEN string: $(fen)"

expect fenToBoard Fen.initialGame == initialBoard
