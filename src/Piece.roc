module [PieceIdx, fromStr, toStr, none, bishop, king, knight, pawn, queen, rook]

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
    S.substr pieces { start: piece, len: 1 } |> Result.withDefault "?" # Cannot fail?

expect toStr none == " "
expect toStr bishop == "b"
expect toStr rook == "r"
