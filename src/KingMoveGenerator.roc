module [generate_moves]

import Board exposing [Bitboard, Board, initial_board]
import Color exposing [Color]
import Fen
import FenParser
import Move exposing [Move]
import Num exposing [bitwise_and]
import Piece
import Square exposing [
    SquareIdx,
    a1_idx,
    b1_idx,
    c1_idx,
    d1_idx,
    e1_idx,
    f1_idx,
    g1_idx,
    h1_idx,
    a2_idx,
    b2_idx,
    c2_idx,
    d2_idx,
    e2_idx,
    f2_idx,
    g2_idx,
    h2_idx,
    a3_idx,
    b3_idx,
    c3_idx,
    d3_idx,
    e3_idx,
    f3_idx,
    g3_idx,
    h3_idx,
    a4_idx,
    b4_idx,
    c4_idx,
    d4_idx,
    e4_idx,
    f4_idx,
    g4_idx,
    h4_idx,
    a5_idx,
    b5_idx,
    c5_idx,
    d5_idx,
    e5_idx,
    f5_idx,
    g5_idx,
    h5_idx,
    a6_idx,
    b6_idx,
    c6_idx,
    d6_idx,
    e6_idx,
    f6_idx,
    g6_idx,
    h6_idx,
    a7_idx,
    b7_idx,
    c7_idx,
    d7_idx,
    e7_idx,
    f7_idx,
    g7_idx,
    h7_idx,
    a8_idx,
    b8_idx,
    c8_idx,
    d8_idx,
    e8_idx,
    f8_idx,
    g8_idx,
    h8_idx,
]
import Util

## All squares a king can move to from a certain square
king_squares = Dict.from_list(
    [
        # Rank 1
        (a1_idx, [a2_idx, b2_idx, b1_idx]),
        (b1_idx, [a1_idx, a2_idx, b2_idx, c2_idx, c1_idx]),
        (c1_idx, [b1_idx, b2_idx, c2_idx, d2_idx, d1_idx]),
        (d1_idx, [c1_idx, c2_idx, d2_idx, e2_idx, e1_idx]),
        (e1_idx, [d1_idx, d2_idx, e2_idx, f2_idx, f1_idx]),
        (f1_idx, [e1_idx, e2_idx, f2_idx, g2_idx, g1_idx]),
        (g1_idx, [f1_idx, f2_idx, g2_idx, h2_idx, h1_idx]),
        (h1_idx, [g1_idx, g2_idx, h2_idx]),

        # Rank 2
        (a2_idx, [a3_idx, b3_idx, b2_idx, b1_idx, a1_idx]),
        (b2_idx, [a2_idx, a3_idx, b3_idx, c3_idx, c2_idx, c1_idx, b1_idx, a1_idx]),
        (c2_idx, [b2_idx, b3_idx, c3_idx, d3_idx, d2_idx, d1_idx, c1_idx, b1_idx]),
        (d2_idx, [c2_idx, c3_idx, d3_idx, e3_idx, e2_idx, e1_idx, d1_idx, c1_idx]),
        (e2_idx, [d2_idx, d3_idx, e3_idx, f3_idx, f2_idx, f1_idx, e1_idx, d1_idx]),
        (f2_idx, [e2_idx, e3_idx, f3_idx, g3_idx, g2_idx, g1_idx, f1_idx, e1_idx]),
        (g2_idx, [f2_idx, f3_idx, g3_idx, h3_idx, h2_idx, h1_idx, g1_idx, f1_idx]),
        (h2_idx, [g2_idx, g3_idx, h3_idx, h1_idx, g1_idx]),

        # Rank 3
        (a3_idx, [a4_idx, b4_idx, b3_idx, b2_idx, a2_idx]),
        (b3_idx, [a3_idx, a4_idx, b4_idx, c4_idx, c3_idx, c2_idx, b2_idx, a2_idx]),
        (c3_idx, [b3_idx, b4_idx, c4_idx, d4_idx, d3_idx, d2_idx, c2_idx, b2_idx]),
        (d3_idx, [c3_idx, c4_idx, d4_idx, e4_idx, e3_idx, e2_idx, d2_idx, c2_idx]),
        (e3_idx, [d3_idx, d4_idx, e4_idx, f4_idx, f3_idx, f2_idx, e2_idx, d2_idx]),
        (f3_idx, [e3_idx, e4_idx, f4_idx, g4_idx, g3_idx, g2_idx, f2_idx, e2_idx]),
        (g3_idx, [f3_idx, f4_idx, g4_idx, h4_idx, h3_idx, h2_idx, g2_idx, f2_idx]),
        (h3_idx, [g3_idx, g4_idx, h4_idx, h2_idx, g2_idx]),

        # Rank 4
        (a4_idx, [a5_idx, b5_idx, b4_idx, b3_idx, a3_idx]),
        (b4_idx, [a4_idx, a5_idx, b5_idx, c5_idx, c4_idx, c3_idx, b3_idx, a3_idx]),
        (c4_idx, [b4_idx, b5_idx, c5_idx, d5_idx, d4_idx, d3_idx, c3_idx, b3_idx]),
        (d4_idx, [c4_idx, c5_idx, d5_idx, e5_idx, e4_idx, e3_idx, d3_idx, c3_idx]),
        (e4_idx, [d4_idx, d5_idx, e5_idx, f5_idx, f4_idx, f3_idx, e3_idx, d3_idx]),
        (f4_idx, [e4_idx, e5_idx, f5_idx, g5_idx, g4_idx, g3_idx, f3_idx, e3_idx]),
        (g4_idx, [f4_idx, f5_idx, g5_idx, h5_idx, h4_idx, h3_idx, g3_idx, f3_idx]),
        (h4_idx, [g4_idx, g5_idx, h5_idx, h3_idx, g3_idx]),

        # Rank 5
        (a5_idx, [a6_idx, b6_idx, b5_idx, b4_idx, a4_idx]),
        (b5_idx, [a5_idx, a6_idx, b6_idx, c6_idx, c5_idx, c4_idx, b4_idx, a4_idx]),
        (c5_idx, [b5_idx, b6_idx, c6_idx, d6_idx, d5_idx, d4_idx, c4_idx, b4_idx]),
        (d5_idx, [c5_idx, c6_idx, d6_idx, e6_idx, e5_idx, e4_idx, d4_idx, c4_idx]),
        (e5_idx, [d5_idx, d6_idx, e6_idx, f6_idx, f5_idx, f4_idx, e4_idx, d4_idx]),
        (f5_idx, [e5_idx, e6_idx, f6_idx, g6_idx, g5_idx, g4_idx, f4_idx, e4_idx]),
        (g5_idx, [f5_idx, f6_idx, g6_idx, h6_idx, h5_idx, h4_idx, g4_idx, f4_idx]),
        (h5_idx, [g5_idx, g6_idx, h6_idx, h4_idx, g4_idx]),

        # Rank 6
        (a6_idx, [a7_idx, b7_idx, b6_idx, b5_idx, a5_idx]),
        (b6_idx, [a6_idx, a7_idx, b7_idx, c7_idx, c6_idx, c5_idx, b5_idx, a5_idx]),
        (c6_idx, [b6_idx, b7_idx, c7_idx, d7_idx, d6_idx, d5_idx, c5_idx, b5_idx]),
        (d6_idx, [c6_idx, c7_idx, d7_idx, e7_idx, e6_idx, e5_idx, d5_idx, c5_idx]),
        (e6_idx, [d6_idx, d7_idx, e7_idx, f7_idx, f6_idx, f5_idx, e5_idx, d5_idx]),
        (f6_idx, [e6_idx, e7_idx, f7_idx, g7_idx, g6_idx, g5_idx, f5_idx, e5_idx]),
        (g6_idx, [f6_idx, f7_idx, g7_idx, h7_idx, h6_idx, h5_idx, g5_idx, f5_idx]),
        (h6_idx, [g6_idx, g7_idx, h7_idx, h5_idx, g5_idx]),

        # Rank 7
        (a7_idx, [a8_idx, b8_idx, b7_idx, b6_idx, a6_idx]),
        (b7_idx, [a7_idx, a8_idx, b8_idx, c8_idx, c7_idx, c6_idx, b6_idx, a6_idx]),
        (c7_idx, [b7_idx, b8_idx, c8_idx, d8_idx, d7_idx, d6_idx, c6_idx, b6_idx]),
        (d7_idx, [c7_idx, c8_idx, d8_idx, e8_idx, e7_idx, e6_idx, d6_idx, c6_idx]),
        (e7_idx, [d7_idx, d8_idx, e8_idx, f8_idx, f7_idx, f6_idx, e6_idx, d6_idx]),
        (f7_idx, [e7_idx, e8_idx, f8_idx, g8_idx, g7_idx, g6_idx, f6_idx, e6_idx]),
        (g7_idx, [f7_idx, f8_idx, g8_idx, h8_idx, h7_idx, h6_idx, g6_idx, f6_idx]),
        (h7_idx, [g7_idx, g8_idx, h8_idx, h6_idx, g6_idx]),

        # Rank 8
        (a8_idx, [b8_idx, b7_idx, a7_idx]),
        (b8_idx, [a8_idx, c8_idx, c7_idx, b7_idx, a7_idx]),
        (c8_idx, [b8_idx, d8_idx, d7_idx, c7_idx, b7_idx]),
        (d8_idx, [c8_idx, e8_idx, e7_idx, d7_idx, c7_idx]),
        (e8_idx, [d8_idx, f8_idx, f7_idx, e7_idx, d7_idx]),
        (f8_idx, [e8_idx, g8_idx, g7_idx, f7_idx, e7_idx]),
        (g8_idx, [f8_idx, h8_idx, h7_idx, g7_idx, f7_idx]),
        (h8_idx, [g8_idx, h7_idx, g7_idx]),
    ],
)

## Castling moves
e1c1 = Move.create_castling(e1_idx, c1_idx)
e1g1 = Move.create_castling(e1_idx, g1_idx)
e8c8 = Move.create_castling(e8_idx, c8_idx)
e8g8 = Move.create_castling(e8_idx, g8_idx)

generate_moves : Board, Bitboard, Bitboard, Color -> List Move
generate_moves = |board, my_pieces, their_pieces, side_to_move|
    # Find from squares
    kings = bitwise_and(my_pieces, board.king)
    king_idxs = Board.bb_to_idxs(kings)
    when List.first(king_idxs) is
        Ok(idx) ->
            generate_moves_from_square(board, my_pieces, their_pieces, idx)
            |> List.concat(generate_castling_moves(board, idx, side_to_move))

        Err(_) -> crash("Should not happen: king not found")

expect generate_moves(initial_board, initial_board.white, initial_board.black, White) == []
expect
    board = FenParser.fen_to_board(Fen.white_can_castle_qs)
    moves = generate_moves(board, board.white, board.black, White) |> to_str
    Set.from_list(moves) == Set.from_list(["e1e2", "e1d2", "e1d1", "e1c1"])

generate_castling_moves : Board, SquareIdx, Color -> List Move
generate_castling_moves = |board, from_idx, side_to_move|
    if side_to_move == White and from_idx == e1_idx then
        if Board.is_castling_allowed(board, e1_idx, c1_idx) then
            if Board.is_castling_allowed(board, e1_idx, g1_idx) then
                [e1c1, e1g1]
            else
                [e1c1]
        else if Board.is_castling_allowed(board, e1_idx, g1_idx) then
            [e1g1]
        else
            [] # Castling not allowed for White
    else if side_to_move == Black and from_idx == e8_idx then
        if Board.is_castling_allowed(board, e8_idx, c8_idx) then
            if Board.is_castling_allowed(board, e8_idx, g8_idx) then
                [e8c8, e8g8]
            else
                [e8c8]
        else if Board.is_castling_allowed(board, e8_idx, g8_idx) then
            [e8g8]
        else
            [] # Castling not allowed for Black
    else
        [] # King not on original square

expect
    board = Util.with_moves(initial_board, ["e2e4", "e7e5", "g1f3", "g8f6", "f1c4", "f8c5"], White)
    moves = generate_castling_moves(board, e1_idx, White) |> to_str
    Set.from_list(moves) == Set.from_list(["e1g1"])

generate_moves_from_square : Board, Bitboard, Bitboard, SquareIdx -> List Move
generate_moves_from_square = |board, my_pieces, their_pieces, from_idx|
    when Dict.get(king_squares, from_idx) is
        Ok(to_idxs) ->
            # Remove squares occupied by my pieces and create moves
            to_idxs
            |> List.keep_if(
                |to_idx|
                    to_id = Square.idx_to_id(to_idx)
                    bitwise_and(my_pieces, to_id) == 0,
            )
            |> List.map(
                |to_idx|
                    create_move(board, their_pieces, from_idx, to_idx),
            )

        Err(_) -> crash("Should not happen: fromIdx not found: ${Num.to_str(from_idx)}")

# After 1. e4 e5 2. Bc4 Bc5
expect
    board = Util.with_moves(initial_board, ["e2e4", "e7e5", "f1c4", "f8c5"], White)
    moves = generate_moves_from_square(board, board.white, board.black, e1_idx) |> to_str
    Set.from_list(moves) == Set.from_list(["e1e2", "e1f1"])

create_move : Board, Bitboard, SquareIdx, SquareIdx -> Move
create_move = |board, their_pieces, from_idx, to_idx|
    to_id = Square.idx_to_id(to_idx)
    if bitwise_and(their_pieces, to_id) != 0 then
        captured = Board.piece_at(board, to_id)
        Move.create_capture(from_idx, to_idx, Piece.king, captured)
    else
        Move.create(from_idx, to_idx, Piece.king)

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

to_str : List Move -> List Str
to_str = |moves|
    List.map(moves, |move| Move.to_str(move))
