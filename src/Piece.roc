module [PieceIdx, fromStr, toStr, toPrettyStr, none, bishop, king, knight, pawn, queen, rook]

import Color exposing [Color]
import S

PieceIdx : U64

none = 0u64
bishop = 1u64
king = 2u64
knight = 3u64
pawn = 4u64
queen = 5u64
rook = 6u64

# Note: Index 0 is 'none'.
pieces = " bknpqr"

fromStr : Str -> Result PieceIdx [SyntaxError]
fromStr = \str ->
    S.indexOf pieces str |> Result.mapErr \_ -> SyntaxError

expect fromStr "b" == Ok bishop
expect fromStr "k" == Ok king
expect fromStr "n" == Ok knight
expect fromStr "p" == Ok pawn
expect fromStr "q" == Ok queen
expect fromStr "r" == Ok rook

toStr : PieceIdx -> Str
toStr = \piece ->
    when S.substr pieces { start: piece, len: 1 } is
        Ok s -> s
        _ -> crash "Should not happen: unknown piece in toStr: $(Num.toStr piece)"

expect toStr none == " "
expect toStr bishop == "b"
expect toStr rook == "r"

toPrettyStr : PieceIdx, Color -> Str
toPrettyStr = \piece, color ->
    when (piece, color) is
        (0, White) -> " "
        (1, White) -> "♗"
        (2, White) -> "♔"
        (3, White) -> "♘"
        (4, White) -> "♙"
        (5, White) -> "♕"
        (6, White) -> "♖"
        (0, Black) -> " "
        (1, Black) -> "♝"
        (2, Black) -> "♚"
        (3, Black) -> "♞"
        (4, Black) -> "♟︎"
        (5, Black) -> "♛"
        (6, Black) -> "♜"
        _ -> crash "Should not happen: unknown piece in toPrettyStr: $(Num.toStr piece)"

expect toPrettyStr knight White == "♘"
expect toPrettyStr queen Black == "♛"
