module [from_list, fen_to_board]

import Board exposing [Board, empty_board, initial_board]
import Color exposing [Color]
import Fen
import Game exposing [Game, initial_game]
import Num exposing [bitwise_or]
import Piece exposing [PieceIdx]
import Square exposing [e4]
import Util

## Parse a FEN string and return a Game initialized from that string.
from_str : Str -> Result Game [IllegalPosition, SyntaxError]
from_str = |fen|
    from_list(Str.split_on(fen, " "))

from_list : List Str -> Result Game [IllegalPosition, SyntaxError]
from_list = |fen|
    result =
        when fen is
            [pieces, active_color, castling_rights, en_passant, half_move_clock, full_move_number] ->
                Ok({ initial_game & board: empty_board })
                |> Result.try(|g| with_pieces(g, pieces))
                |> Result.try(|g| with_active_color(g, active_color))
                |> Result.try(|g| with_castling_rights(g, castling_rights))
                |> Result.try(|g| with_en_passant(g, en_passant))
                |> Result.try(|g| with_move_number(g, full_move_number))
                |> Result.try(|g| with_half_move_clock(g, half_move_clock))

            _ -> Err(SyntaxError)
    # Check that parsing the FEN string resulted in a legal position
    result
    |> Result.try(|g| if Board.is_legal(g.board) then Ok(g) else Err(SyntaxError))
    |> Result.map_err(|_| SyntaxError)

expect from_str(Fen.initial_game) == Ok(initial_game)
expect
    board_from_fen =
        from_str(Fen.fools_mate)
        |> Result.map_ok(.board)
        |> Result.with_default(initial_board)
    board_from_moves = Util.with_moves(initial_board, ["f2f3", "e7e5", "g2g4", "d8h4"], White)
    board_from_fen == board_from_moves
expect
    board_from_fen =
        from_str(Fen.scholars_mate)
        |> Result.map_ok(.board)
        |> Result.with_default(initial_board)
    board_from_moves = Util.with_moves(initial_board, ["e2e4", "e7e5", "f1c4", "f8c5", "d1h5", "g8f6", "h5f7"], White)
    board_from_fen == board_from_moves
expect from_str(Fen.syntax_error) == Err(SyntaxError)

with_pieces = |game, str|
    ranks = Str.split_on(str, "/")
    if List.len(ranks) == 8 then
        with_ranks(game.board, ranks) |> Result.map_ok(|b| { game & board: b })
    else
        Err(SyntaxError)

with_ranks : Board, List Str -> Result Board [SyntaxError]
with_ranks = |board, ranks|
    List.walk_try(
        ranks,
        { b: board, r: 0 },
        |state, str|
            when with_rank(state.b, state.r, str) is
                Ok(b) -> Ok({ b: b, r: state.r + 1 })
                Err(SyntaxError) -> Err(SyntaxError),
    )
    |> Result.map_ok(.b)

with_rank : Board, U8, Str -> Result Board [SyntaxError]
with_rank = |board, rank, str|
    List.walk_try(Str.to_utf8(str), { b: board, r: rank, f: 0 }, iter)
    |> Result.map_ok(.b)

iter : { b : Board, r : U8, f : U8 }, U8 -> Result { b : Board, r : U8, f : U8 } [SyntaxError]
iter = |state, byte|
    when byte is
        66 -> Ok(with_piece(state, Piece.bishop, White))
        75 -> Ok(with_piece(state, Piece.king, White))
        78 -> Ok(with_piece(state, Piece.knight, White))
        80 -> Ok(with_piece(state, Piece.pawn, White))
        81 -> Ok(with_piece(state, Piece.queen, White))
        82 -> Ok(with_piece(state, Piece.rook, White))
        98 -> Ok(with_piece(state, Piece.bishop, Black))
        107 -> Ok(with_piece(state, Piece.king, Black))
        110 -> Ok(with_piece(state, Piece.knight, Black))
        112 -> Ok(with_piece(state, Piece.pawn, Black))
        113 -> Ok(with_piece(state, Piece.queen, Black))
        114 -> Ok(with_piece(state, Piece.rook, Black))
        # Number of empty squares
        n if n >= 48 and n <= 56 -> Ok({ state & f: state.f + n - 48 })
        _ -> Err(SyntaxError)

expect
    # In FEN strings, the rank numbers are reversed, hence rank 4 below
    when iter({ b: initial_board, r: 4, f: 4 }, 66) is
        Ok(state) ->
            (state.r == 4)
            and (state.f == 5)
            and (Board.piece_at(state.b, e4) == Piece.bishop)
            and (Board.color_at(state.b, e4) == Ok(White))

        Err(_) -> 1 == 0

expect
    result = iter({ b: initial_board, r: 4, f: 4 }, 17)
    result == Err(SyntaxError)

with_piece : { b : Board, r : U8, f : U8 }, PieceIdx, Color -> { b : Board, r : U8, f : U8 }
with_piece = |state, piece, color|
    board = state.b
    square_id = Square.fr_to_id(state.f, (7 - state.r))
    board_with_piece =
        when piece is
            1 -> { board & bishop: bitwise_or(board.bishop, square_id) }
            2 -> { board & king: bitwise_or(board.king, square_id) }
            3 -> { board & knight: bitwise_or(board.knight, square_id) }
            4 -> { board & pawn: bitwise_or(board.pawn, square_id) }
            5 -> { board & queen: bitwise_or(board.queen, square_id) }
            6 -> { board & rook: bitwise_or(board.rook, square_id) }
            _ -> crash("Should not happen: unknown piece in withPiece: ${Num.to_str(piece)}")
    board_with_color =
        when color is
            White -> { board_with_piece & white: bitwise_or(board.white, square_id) }
            Black -> { board_with_piece & black: bitwise_or(board.black, square_id) }
    { state & b: board_with_color, f: state.f + 1 }

with_active_color = |game, str|
    when Str.trim(str) is
        "w" -> Ok({ game & active_color: White })
        "b" -> Ok({ game & active_color: Black })
        _ -> Err(SyntaxError)

expect
    game = with_active_color(initial_game, "b")
    game == Ok({ initial_game & active_color: Black })
expect with_active_color(initial_game, "x") == Err(SyntaxError)

with_castling_rights = |game, str|
    game
    |> |g| if Str.contains(str, "K") then { g & board: Board.with_castling_rights(g.board, White, Piece.king) } else g
    |> |g| if Str.contains(str, "Q") then { g & board: Board.with_castling_rights(g.board, White, Piece.queen) } else g
    |> |g| if Str.contains(str, "k") then { g & board: Board.with_castling_rights(g.board, Black, Piece.king) } else g
    |> |g| if Str.contains(str, "q") then { g & board: Board.with_castling_rights(g.board, Black, Piece.queen) } else g
    |> Ok

with_move_number = |game, str|
    when Str.trim(str) |> Str.to_u64 is
        Ok(number) -> Ok({ game & move_number: number })
        _ -> Err(SyntaxError)

expect
    game = with_move_number(initial_game, "82")
    game == Ok({ initial_game & move_number: 82 })

with_half_move_clock = |game, str|
    when Str.trim(str) |> Str.to_u64 is
        Ok(number) -> Ok({ game & board: Board.with_half_move_clock(game.board, number) })
        _ -> Err(SyntaxError)

with_en_passant = |game, str|
    Str.trim(str)
    |> |trimmed| if trimmed == "-" then Ok(0) else Square.from_str(trimmed)
    |> Result.map_ok(|square| { game & board: Board.with_en_passant_square(game.board, square) })

## Setup a board from the given FEN string.
fen_to_board : Str -> Board
fen_to_board = |fen|
    when from_str(fen) is
        Ok(game) -> game.board
        _ -> crash("Should not happen: invalid FEN string: ${fen}")

expect fen_to_board(Fen.initial_game) == initial_board
