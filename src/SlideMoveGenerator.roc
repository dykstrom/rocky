module [generate_moves_from_square, n, ne, e, se, s, sw, w, nw]

import Board exposing [Bitboard, Board, initial_board]
import L
import Move exposing [Move]
import Num exposing [bitwise_and, bitwise_or]
import Piece exposing [PieceIdx]
import Square exposing [SquareIdx, c6_idx, e4_idx, f4_idx, g4_idx, a1, a2, a3, a4, a5, a6, a7, a8, h1, h2, h3, h4, h5, h6, h7, h8]

n = 8
ne = 9
e = 1
se = -7
s = -8
sw = -9
w = -1
nw = 7

file_a = L.or_list([a1, a2, a3, a4, a5, a6, a7, a8])
file_h = L.or_list([h1, h2, h3, h4, h5, h6, h7, h8])

generate_moves_from_square : Board, Bitboard, Bitboard, SquareIdx, PieceIdx, List I64 -> List Move
generate_moves_from_square = |board, my_pieces, their_pieces, from_idx, moved_idx, steps|
    expect
        List.len(steps) > 0

    iter : I64, I64, Bitboard, List Move -> List Move
    iter = |to_idx, step, blocked, list|
        if to_idx < 0 or to_idx > 63 then
            list
        else
            to_square = Square.idx_to_id(Num.to_u64(to_idx))
            if bitwise_and(to_square, blocked) != 0 then
                list
            else if bitwise_and(to_square, their_pieces) != 0 then
                captured = Board.piece_at(board, to_square)
                move = Move.create_capture(from_idx, Num.to_u64(to_idx), moved_idx, captured)
                List.append(list, move)
            else
                move = Move.create(from_idx, Num.to_u64(to_idx), moved_idx)
                iter((to_idx + step), step, blocked, List.append(list, move))

    List.walk(
        steps,
        [],
        |list, step|
            blocking_file =
                if step == -7 or step == 1 or step == 9 then
                    file_a
                else if step == -9 or step == -1 or step == 7 then
                    file_h
                else
                    0
            blocked = bitwise_or(my_pieces, blocking_file)
            idx = Num.to_i64(from_idx)
            tmp = iter((idx + step), step, blocked, [])
            List.concat(list, tmp),
    )

# Slide east
expect
    moves = run_test(Piece.rook, g4_idx, [e])
    Set.from_list(moves) == Set.from_list(["g4h4"])
# Slide west
expect
    moves = run_test(Piece.rook, c6_idx, [w])
    Set.from_list(moves) == Set.from_list(["c6b6", "c6a6"])
# Slide east and west
expect
    moves = run_test(Piece.rook, e4_idx, [e, w])
    Set.from_list(moves) == Set.from_list(["e4f4", "e4g4", "e4h4", "e4d4", "e4c4", "e4b4", "e4a4"])
# Slide north
expect
    moves = run_test(Piece.rook, g4_idx, [n])
    Set.from_list(moves) == Set.from_list(["g4g5", "g4g6", "g4g7"])
# Slide south
expect
    moves = run_test(Piece.rook, c6_idx, [s])
    Set.from_list(moves) == Set.from_list(["c6c5", "c6c4", "c6c3"])
# Slide northeast
expect
    moves = run_test(Piece.bishop, f4_idx, [ne])
    Set.from_list(moves) == Set.from_list(["f4g5", "f4h6"])
# Slide southeast
expect
    moves = run_test(Piece.bishop, f4_idx, [se])
    Set.from_list(moves) == Set.from_list(["f4g3"])
# Slide southwest
expect
    moves = run_test(Piece.bishop, f4_idx, [sw])
    Set.from_list(moves) == Set.from_list(["f4e3"])
# Slide northwest
expect
    moves = run_test(Piece.bishop, f4_idx, [nw])
    Set.from_list(moves) == Set.from_list(["f4e5", "f4d6", "f4c7"])
# Slide in all directions
expect
    moves = run_test(Piece.bishop, f4_idx, [n, ne, e, se, s, sw, w, nw])
    Set.from_list(moves) == Set.from_list(["f4f5", "f4f6", "f4f7", "f4g5", "f4h6", "f4g4", "f4h4", "f4g3", "f4f3", "f4e3", "f4e4", "f4d4", "f4c4", "f4b4", "f4a4", "f4e5", "f4d6", "f4c7"])

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

run_test : PieceIdx, SquareIdx, List I64 -> List Str
run_test = |moved_idx, from_idx, steps|
    generate_moves_from_square(initial_board, initial_board.white, initial_board.black, from_idx, moved_idx, steps)
    |> to_str

to_str : List Move -> List Str
to_str = |moves|
    List.map(moves, |move| Move.to_str(move))
