module [generate_moves]

import Board exposing [Bitboard, Board, initial_board]
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

## All squares a knight can move to from a certain square
knight_squares = Dict.from_list(
    [
        # Rank 1
        (a1_idx, [b3_idx, c2_idx]),
        (b1_idx, [a3_idx, c3_idx, d2_idx]),
        (c1_idx, [a2_idx, b3_idx, d3_idx, e2_idx]),
        (d1_idx, [b2_idx, c3_idx, e3_idx, f2_idx]),
        (e1_idx, [c2_idx, d3_idx, f3_idx, g2_idx]),
        (f1_idx, [d2_idx, e3_idx, g3_idx, h2_idx]),
        (g1_idx, [e2_idx, f3_idx, h3_idx]),
        (h1_idx, [f2_idx, g3_idx]),

        # Rank 2
        (a2_idx, [b4_idx, c3_idx, c1_idx]),
        (b2_idx, [a4_idx, c4_idx, d3_idx, d1_idx]),
        (c2_idx, [a1_idx, a3_idx, b4_idx, d4_idx, e3_idx, e1_idx]),
        (d2_idx, [b1_idx, b3_idx, c4_idx, e4_idx, f3_idx, f1_idx]),
        (e2_idx, [c1_idx, c3_idx, d4_idx, f4_idx, g3_idx, g1_idx]),
        (f2_idx, [d1_idx, d3_idx, e4_idx, g4_idx, h3_idx, h1_idx]),
        (g2_idx, [e1_idx, e3_idx, f4_idx, h4_idx]),
        (h2_idx, [f1_idx, f3_idx, g4_idx]),

        # Rank 3
        (a3_idx, [b5_idx, c4_idx, c2_idx, b1_idx]),
        (b3_idx, [a1_idx, a5_idx, c5_idx, d4_idx, d2_idx, c1_idx]),
        (c3_idx, [b1_idx, a2_idx, a4_idx, b5_idx, d5_idx, e4_idx, e2_idx, d1_idx]),
        (d3_idx, [c1_idx, b2_idx, b4_idx, c5_idx, e5_idx, f4_idx, f2_idx, e1_idx]),
        (e3_idx, [d1_idx, c2_idx, c4_idx, d5_idx, f5_idx, g4_idx, g2_idx, f1_idx]),
        (f3_idx, [e1_idx, d2_idx, d4_idx, e5_idx, g5_idx, h4_idx, h2_idx, g1_idx]),
        (g3_idx, [f1_idx, e2_idx, e4_idx, f5_idx, h5_idx, h1_idx]),
        (h3_idx, [g1_idx, f2_idx, f4_idx, g5_idx]),

        # Rank 4
        (a4_idx, [b6_idx, c5_idx, c3_idx, b2_idx]),
        (b4_idx, [a2_idx, a6_idx, c6_idx, d5_idx, d3_idx, c2_idx]),
        (c4_idx, [b2_idx, a3_idx, a5_idx, b6_idx, d6_idx, e5_idx, e3_idx, d2_idx]),
        (d4_idx, [c2_idx, b3_idx, b5_idx, c6_idx, e6_idx, f5_idx, f3_idx, e2_idx]),
        (e4_idx, [d2_idx, c3_idx, c5_idx, d6_idx, f6_idx, g5_idx, g3_idx, f2_idx]),
        (f4_idx, [e2_idx, d3_idx, d5_idx, e6_idx, g6_idx, h5_idx, h3_idx, g2_idx]),
        (g4_idx, [f2_idx, e3_idx, e5_idx, f6_idx, h6_idx, h2_idx]),
        (h4_idx, [g2_idx, f3_idx, f5_idx, g6_idx]),

        # Rank 5
        (a5_idx, [b7_idx, c6_idx, c4_idx, b3_idx]),
        (b5_idx, [a3_idx, a7_idx, c7_idx, d6_idx, d4_idx, c3_idx]),
        (c5_idx, [b3_idx, a4_idx, a6_idx, b7_idx, d7_idx, e6_idx, e4_idx, d3_idx]),
        (d5_idx, [c3_idx, b4_idx, b6_idx, c7_idx, e7_idx, f6_idx, f4_idx, e3_idx]),
        (e5_idx, [d3_idx, c4_idx, c6_idx, d7_idx, f7_idx, g6_idx, g4_idx, f3_idx]),
        (f5_idx, [e3_idx, d4_idx, d6_idx, e7_idx, g7_idx, h6_idx, h4_idx, g3_idx]),
        (g5_idx, [f3_idx, e4_idx, e6_idx, f7_idx, h7_idx, h3_idx]),
        (h5_idx, [g3_idx, f4_idx, f6_idx, g7_idx]),

        # Rank 6
        (a6_idx, [b8_idx, c7_idx, c5_idx, b4_idx]),
        (b6_idx, [a4_idx, a8_idx, c8_idx, d7_idx, d5_idx, c4_idx]),
        (c6_idx, [b4_idx, a5_idx, a7_idx, b8_idx, d8_idx, e7_idx, e5_idx, d4_idx]),
        (d6_idx, [c4_idx, b5_idx, b7_idx, c8_idx, e8_idx, f7_idx, f5_idx, e4_idx]),
        (e6_idx, [d4_idx, c5_idx, c7_idx, d8_idx, f8_idx, g7_idx, g5_idx, f4_idx]),
        (f6_idx, [e4_idx, d5_idx, d7_idx, e8_idx, g8_idx, h7_idx, h5_idx, g4_idx]),
        (g6_idx, [f4_idx, e5_idx, e7_idx, f8_idx, h8_idx, h4_idx]),
        (h6_idx, [g4_idx, f5_idx, f7_idx, g8_idx]),

        # Rank 7
        (a7_idx, [c8_idx, c6_idx, b5_idx]),
        (b7_idx, [d8_idx, d6_idx, c5_idx, a5_idx]),
        (c7_idx, [b5_idx, a6_idx, a8_idx, e8_idx, e6_idx, d5_idx]),
        (d7_idx, [c5_idx, b6_idx, b8_idx, f8_idx, f6_idx, e5_idx]),
        (e7_idx, [d5_idx, c6_idx, c8_idx, g8_idx, g6_idx, f5_idx]),
        (f7_idx, [e5_idx, d6_idx, d8_idx, h8_idx, h6_idx, g5_idx]),
        (g7_idx, [f5_idx, e6_idx, e8_idx, h5_idx]),
        (h7_idx, [g5_idx, f6_idx, f8_idx]),

        # Rank 8
        (a8_idx, [c7_idx, b6_idx]),
        (b8_idx, [a6_idx, d7_idx, c6_idx]),
        (c8_idx, [b6_idx, a7_idx, e7_idx, d6_idx]),
        (d8_idx, [c6_idx, b7_idx, f7_idx, e6_idx]),
        (e8_idx, [d6_idx, c7_idx, g7_idx, f6_idx]),
        (f8_idx, [e6_idx, d7_idx, h7_idx, g6_idx]),
        (g8_idx, [f6_idx, e7_idx, h6_idx]),
        (h8_idx, [g6_idx, f7_idx]),
    ],
)

generate_moves : Board, Bitboard, Bitboard -> List Move
generate_moves = |board, my_pieces, their_pieces|
    # Find from squares
    knights = bitwise_and(my_pieces, board.knight)
    knight_idxs = Board.bb_to_idxs(knights)
    # For each from square, create a knight move
    List.walk(
        knight_idxs,
        [],
        |list, idx|
            List.concat(list, generate_moves_from_square(board, my_pieces, their_pieces, idx)),
    )

expect
    moves = generate_moves(initial_board, initial_board.white, initial_board.black) |> to_str
    Set.from_list(moves) == Set.from_list(["b1a3", "b1c3", "g1f3", "g1h3"])

generate_moves_from_square : Board, Bitboard, Bitboard, SquareIdx -> List Move
generate_moves_from_square = |board, my_pieces, their_pieces, from_idx|
    when Dict.get(knight_squares, from_idx) is
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

# Initial position White
expect
    moves = generate_moves_from_square(initial_board, initial_board.white, initial_board.black, g1_idx) |> to_str
    Set.from_list(moves) == Set.from_list(["g1f3", "g1h3"])
# Initial position Black
expect
    moves = generate_moves_from_square(initial_board, initial_board.black, initial_board.white, b8_idx) |> to_str
    Set.from_list(moves) == Set.from_list(["b8a6", "b8c6"])

create_move : Board, Bitboard, SquareIdx, SquareIdx -> Move
create_move = |board, their_pieces, from_idx, to_idx|
    to_id = Square.idx_to_id(to_idx)
    if bitwise_and(their_pieces, to_id) != 0 then
        captured = Board.piece_at(board, to_id)
        Move.create_capture(from_idx, to_idx, Piece.knight, captured)
    else
        Move.create(from_idx, to_idx, Piece.knight)

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

to_str : List Move -> List Str
to_str = |moves|
    List.map(moves, |move| Move.to_str(move))
