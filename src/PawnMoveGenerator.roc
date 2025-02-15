module [generate_moves]

import Board exposing [Bitboard, Board, initial_board]
import Fen
import FenParser
import L
import Move exposing [Move]
import Num exposing [bitwise_and, bitwise_not, bitwise_or, shift_left_by, shift_right_zf_by]
import Piece
import Square exposing [SquareIdx, a8_idx, h1_idx, a1, h1, a2, b2, c2, d2, e2, f2, g2, h2, a3, h3, a4, h4, a5, h5, a6, h6, a7, b7, c7, d7, e7, f7, g7, h7, a8, h8]
import Util

rank2 = L.or_list([a2, b2, c2, d2, e2, f2, g2, h2])
rank7 = L.or_list([a7, b7, c7, d7, e7, f7, g7, h7])

files_ag = bitwise_not(L.or_list([h1, h2, h3, h4, h5, h6, h7, h8]))
files_bh = bitwise_not(L.or_list([a1, a2, a3, a4, a5, a6, a7, a8]))

promotion_pieces = [Piece.bishop, Piece.knight, Piece.queen, Piece.rook]

generate_moves : Board, Bitboard -> List Move
generate_moves = |board, their_pieces|
    if bitwise_and(board.white, their_pieces) == 0 then
        generate_white_moves(board, their_pieces)
    else
        generate_black_moves(board, their_pieces)

expect
    board = FenParser.fen_to_board(Fen.white_can_promote)
    moves = generate_moves(board, board.black) |> to_str
    Set.from_list(moves) == Set.from_list(["b2b4", "c2c4", "e2e4", "f2f4", "g2g4", "h2h4", "b2a3", "b2b3", "c2c3", "e2e3", "f2f3", "g2g3", "h2h3", "e7d8b", "e7d8n", "e7d8q", "e7d8r", "e7f8b", "e7f8n", "e7f8q", "e7f8r"])
expect
    board = FenParser.fen_to_board(Fen.black_can_capture_ep)
    moves = generate_moves(board, board.white) |> to_str
    Set.from_list(moves) == Set.from_list(["b7b5", "c7c5", "e7e5", "f7f5", "g7g5", "h7h5", "b7b6", "c7c6", "e7e6", "f7f6", "g7g6", "h7h6", "a4a3", "a4b3", "c7d6", "e7d6"])

generate_white_moves : Board, Bitboard -> List Move
generate_white_moves = |board, their_pieces|
    occupied = bitwise_or(board.white, board.black)
    free = bitwise_not(occupied)
    my_pawns = bitwise_and(board.white, board.pawn)

    # First moves
    pawns_on_rank2 = bitwise_and(my_pawns, rank2)
    fm_from_squares = bitwise_and(pawns_on_rank2, shift_right_zf_by(free, 8))
    fm_to_squares = bitwise_and(shift_left_by(fm_from_squares, 16), free)
    fm_to_idxs = Board.bb_to_idxs(fm_to_squares)
    fm_moves = List.map(
        fm_to_idxs,
        |to_idx|
            Move.create((to_idx - 16), to_idx, Piece.pawn),
    )

    # Normal moves
    nm_to_squares = bitwise_and(shift_left_by(my_pawns, 8), free)
    nm_to_idxs = Board.bb_to_idxs(nm_to_squares)
    nm_moves =
        List.keep_if(nm_to_idxs, is_normal_rank)
        |> List.map(|to_idx| Move.create((to_idx - 8), to_idx, Piece.pawn))

    # Promotion moves
    pr_moves = create_promotion_moves(nm_to_idxs, |to_idx| to_idx - 8)

    # Capture right
    cr_to_squares = bitwise_and(bitwise_and(shift_left_by(my_pawns, 9), files_bh), their_pieces)
    cr_to_idxs = Board.bb_to_idxs(cr_to_squares)
    cr_moves =
        List.keep_if(cr_to_idxs, is_normal_rank)
        |> List.map(
            |to_idx|
                captured = Board.piece_at(board, Square.idx_to_id(to_idx))
                Move.create_capture((to_idx - 9), to_idx, Piece.pawn, captured),
        )

    # Capture right and promote
    crp_moves = create_capture_promotion_moves(board, cr_to_idxs, |to_idx| to_idx - 9)

    # Capture left
    cl_to_squares = bitwise_and(bitwise_and(shift_left_by(my_pawns, 7), files_ag), their_pieces)
    cl_to_idxs = Board.bb_to_idxs(cl_to_squares)
    cl_moves =
        List.keep_if(cl_to_idxs, is_normal_rank)
        |> List.map(
            |to_idx|
                captured = Board.piece_at(board, Square.idx_to_id(to_idx))
                Move.create_capture((to_idx - 7), to_idx, Piece.pawn, captured),
        )

    # Capture left and promote
    clp_moves = create_capture_promotion_moves(board, cl_to_idxs, |to_idx| to_idx - 7)

    # 'En passant' captures
    ep_moves = if Board.is_en_passant_allowed(board) then create_white_en_passant_moves(board, my_pawns) else []

    fm_moves
    |> List.concat(nm_moves)
    |> List.concat(pr_moves)
    |> List.concat(cr_moves)
    |> List.concat(crp_moves)
    |> List.concat(cl_moves)
    |> List.concat(clp_moves)
    |> List.concat(ep_moves)

# Initial position
expect
    moves = generate_white_moves(initial_board, initial_board.black) |> to_str
    Set.from_list(moves) == Set.from_list(["a2a4", "b2b4", "c2c4", "d2d4", "e2e4", "f2f4", "g2g4", "h2h4", "a2a3", "b2b3", "c2c3", "d2d3", "e2e3", "f2f3", "g2g3", "h2h3"])
# After 1. e4 d5
expect
    board = Util.with_moves(initial_board, ["e2e4", "d7d5"], White)
    moves = generate_white_moves(board, board.black) |> to_str
    Set.from_list(moves) == Set.from_list(["a2a4", "b2b4", "c2c4", "d2d4", "f2f4", "g2g4", "h2h4", "a2a3", "b2b3", "c2c3", "d2d3", "f2f3", "g2g3", "h2h3", "e4e5", "e4d5"])

generate_black_moves : Board, Bitboard -> List Move
generate_black_moves = |board, their_pieces|
    occupied = bitwise_or(board.white, board.black)
    free = bitwise_not(occupied)
    my_pawns = bitwise_and(board.black, board.pawn)

    # First moves
    pawns_on_rank7 = bitwise_and(my_pawns, rank7)
    fm_from_squares = bitwise_and(pawns_on_rank7, shift_left_by(free, 8))
    fm_to_squares = bitwise_and(shift_right_zf_by(fm_from_squares, 16), free)
    fm_to_idxs = Board.bb_to_idxs(fm_to_squares)
    fm_moves = List.map(
        fm_to_idxs,
        |to_idx|
            Move.create((to_idx + 16), to_idx, Piece.pawn),
    )

    # Normal moves
    nm_to_squares = bitwise_and(shift_right_zf_by(my_pawns, 8), free)
    nm_to_idxs = Board.bb_to_idxs(nm_to_squares)
    nm_moves =
        List.keep_if(nm_to_idxs, is_normal_rank)
        |> List.map(|to_idx| Move.create((to_idx + 8), to_idx, Piece.pawn))

    # Promotion moves
    pr_moves = create_promotion_moves(nm_to_idxs, |to_idx| to_idx + 8)

    # Capture right
    cr_to_squares = bitwise_and(bitwise_and(shift_right_zf_by(my_pawns, 7), files_bh), their_pieces)
    cr_to_idxs = Board.bb_to_idxs(cr_to_squares)
    cr_moves =
        List.keep_if(cr_to_idxs, is_normal_rank)
        |> List.map(
            |to_idx|
                captured = Board.piece_at(board, Square.idx_to_id(to_idx))
                Move.create_capture((to_idx + 7), to_idx, Piece.pawn, captured),
        )

    # Capture right and promote
    crp_moves = create_capture_promotion_moves(board, cr_to_idxs, |to_idx| to_idx + 7)

    # Capture left
    cl_to_squares = bitwise_and(bitwise_and(shift_right_zf_by(my_pawns, 9), files_ag), their_pieces)
    cl_to_idxs = Board.bb_to_idxs(cl_to_squares)
    cl_moves =
        List.keep_if(cl_to_idxs, is_normal_rank)
        |> List.map(
            |to_idx|
                captured = Board.piece_at(board, Square.idx_to_id(to_idx))
                Move.create_capture((to_idx + 9), to_idx, Piece.pawn, captured),
        )

    # Capture left and promote
    clp_moves = create_capture_promotion_moves(board, cl_to_idxs, |to_idx| to_idx + 9)

    # 'En passant' captures
    ep_moves = if Board.is_en_passant_allowed(board) then create_black_en_passant_moves(board, my_pawns) else []

    fm_moves
    |> List.concat(nm_moves)
    |> List.concat(pr_moves)
    |> List.concat(cr_moves)
    |> List.concat(crp_moves)
    |> List.concat(cl_moves)
    |> List.concat(clp_moves)
    |> List.concat(ep_moves)

# After 1. e4
expect
    board = Util.with_moves(initial_board, ["e2e4"], White)
    moves = generate_black_moves(board, initial_board.white) |> to_str
    Set.from_list(moves) == Set.from_list(["a7a5", "b7b5", "c7c5", "d7d5", "e7e5", "f7f5", "g7g5", "h7h5", "a7a6", "b7b6", "c7c6", "d7d6", "e7e6", "f7f6", "g7g6", "h7h6"])
# After 1. e4 d5 2. Nf3
expect
    board = Util.with_moves(initial_board, ["e2e4", "d7d5", "g1f3"], White)
    moves = generate_black_moves(board, board.white) |> to_str
    Set.from_list(moves) == Set.from_list(["a7a5", "b7b5", "c7c5", "e7e5", "f7f5", "g7g5", "h7h5", "a7a6", "b7b6", "c7c6", "e7e6", "f7f6", "g7g6", "h7h6", "d5d4", "d5e4"])
# After 1. e4 e5
expect
    board = Util.with_moves(initial_board, ["e2e4", "e7e5"], White)
    # When calculating mobility score, we generate moves also for the side that just moved
    moves = generate_black_moves(board, board.white) |> to_str
    Set.from_list(moves) == Set.from_list(["a7a5", "b7b5", "c7c5", "d7d5", "f7f5", "g7g5", "h7h5", "a7a6", "b7b6", "c7c6", "d7d6", "f7f6", "g7g6", "h7h6"])

## Return true if the given square is on a normal rank, that is, not a promotion rank.
is_normal_rank : SquareIdx -> Bool
is_normal_rank = |to_idx|
    to_idx > h1_idx and to_idx < a8_idx

## Create all possible promotion moves landing on the 'to square' indices in the list.
## The function f converts a 'to square' index to the corresponding 'from square' index.
create_promotion_moves : List SquareIdx, (SquareIdx -> SquareIdx) -> List Move
create_promotion_moves = |to_idxs, f|
    List.drop_if(to_idxs, is_normal_rank)
    |> List.walk(
        [],
        |list, to_idx|
            List.concat(list, List.map(promotion_pieces, |promoted| Move.create_promotion(f(to_idx), to_idx, promoted))),
    )

create_capture_promotion_moves : Board, List SquareIdx, (SquareIdx -> SquareIdx) -> List Move
create_capture_promotion_moves = |board, to_idxs, f|
    List.drop_if(to_idxs, is_normal_rank)
    |> List.walk(
        [],
        |list, to_idx|
            captured = Board.piece_at(board, Square.idx_to_id(to_idx))
            List.concat(list, List.map(promotion_pieces, |promoted| Move.create_capture_promotion(f(to_idx), to_idx, captured, promoted))),
    )

create_white_en_passant_moves : Board, Bitboard -> List Move
create_white_en_passant_moves = |board, my_pawns|
    en_passant_idx = Board.en_passant_square(board)
    pawn_idxs = Board.bb_to_idxs(my_pawns)
    # Pawn must start on fifth row to make an 'en passant' capture
    List.keep_if(pawn_idxs, |idx| (idx // 8 == 4) and (en_passant_idx == idx + 7 or en_passant_idx == idx + 9))
    |> List.map(
        |from_idx|
            Move.create_en_passant(from_idx, en_passant_idx),
    )

create_black_en_passant_moves : Board, Bitboard -> List Move
create_black_en_passant_moves = |board, my_pawns|
    en_passant_idx = Board.en_passant_square(board)
    pawn_idxs = Board.bb_to_idxs(my_pawns)
    # Pawn must start on fourth row to make an 'en passant' capture
    # Add to enPassantIdx instead of subtract from idx to avoid U64 underflow in case idx == 8 (pawn on square a2)
    # The && operator is not a short-circuit operator in Roc
    List.keep_if(pawn_idxs, |idx| (idx // 8 == 3) and (en_passant_idx + 7 == idx or en_passant_idx + 9 == idx))
    |> List.map(
        |from_idx|
            Move.create_en_passant(from_idx, en_passant_idx),
    )

expect
    board = Util.with_moves(initial_board, ["e2e4"], White)
    moves = create_black_en_passant_moves(board, Square.d4) |> to_str
    Set.from_list(moves) == Set.from_list(["d4e3"])
expect
    board = Util.with_moves(initial_board, ["e2e4", "e7e5"], White)
    moves = create_black_en_passant_moves(board, bitwise_and(board.pawn, board.black)) |> to_str
    Set.from_list(moves) == Set.from_list([])
expect
    board = FenParser.fen_to_board(Fen.crash_in_generate_black_ep)
    moves = create_black_en_passant_moves(board, L.or_list([a2, a7, b7])) |> to_str
    Set.from_list(moves) == Set.from_list([])

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

to_str : List Move -> List Str
to_str = |moves|
    List.map(moves, |move| Move.to_str(move))
