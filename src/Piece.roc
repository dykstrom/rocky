module [PieceIdx, from_str, to_str, to_pretty_str, none, bishop, king, knight, pawn, queen, rook]

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

from_str : Str -> Result PieceIdx [SyntaxError]
from_str = |str|
    S.index_of(pieces, str) |> Result.map_err(|_| SyntaxError)

expect from_str("b") == Ok(bishop)
expect from_str("k") == Ok(king)
expect from_str("n") == Ok(knight)
expect from_str("p") == Ok(pawn)
expect from_str("q") == Ok(queen)
expect from_str("r") == Ok(rook)

to_str : PieceIdx -> Str
to_str = |piece|
    when S.substr(pieces, { start: piece, len: 1 }) is
        Ok(s) -> s
        _ -> crash("Should not happen: unknown piece in toStr: ${Num.to_str(piece)}")

expect to_str(none) == " "
expect to_str(bishop) == "b"
expect to_str(rook) == "r"

to_pretty_str : PieceIdx, Color -> Str
to_pretty_str = |piece, color|
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
        _ -> crash("Should not happen: unknown piece in toPrettyStr: ${Num.to_str(piece)}")

expect to_pretty_str(knight, White) == "♘"
expect to_pretty_str(queen, Black) == "♛"
