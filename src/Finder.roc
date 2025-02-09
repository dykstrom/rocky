module [find_move]

import Board exposing [Board, initial_board]
import Checker
import Color exposing [Color]
import Evaluator exposing [illegal_check_value, checkmate_value, draw_value]
import Fen
import FenParser
import MoveGenerator
import Move exposing [Move]
import Util

initial_alpha = -3_000_000
initial_beta = 3_000_000

## Find the best move for the side to move. The best move is the
## move with the highest score for the side to move. It will be
## positive if the side to move is in the lead.
find_move : Board, List Board, Color -> [Draw, Mated, FoundMove { move : Move, score : I64 }]
find_move = |board, board_history, side_to_move|
    # Decorate the initial board with the possible moves
    board_with_moves = MoveGenerator.with_moves(board)
    { move, score } = alpha_beta(board_with_moves, board_history, side_to_move, 3, -initial_beta, -initial_alpha)
    if move == 0 then
        if Checker.is_check(board_with_moves, side_to_move) then Mated else Draw
    else
        FoundMove({ move, score })

# White captures 2. dxe5
expect
    { board, history } = Util.with_history(initial_board, ["d2d4", "e7e5"], White)
    when find_move(board, history, White) is
        FoundMove({ move }) -> Move.to_str(move) == "d4e5"
        _ -> Bool.false
# Fool's mate in one
expect
    board = FenParser.fen_to_board(Fen.fools_mate_in_one)
    when find_move(board, [], Black) is
        FoundMove({ move }) -> Move.to_str(move) == "d8h4"
        _ -> Bool.false
# Black has only one legal move
expect
    board = FenParser.fen_to_board(Fen.mate_in_two)
    when find_move(board, [], Black) is
        FoundMove({ move }) -> Move.to_str(move) == "e7d8"
        _ -> Bool.false
# White cannot castle king-side
expect
    board = FenParser.fen_to_board(Fen.white_cannot_castle)
    when find_move(board, [], White) is
        FoundMove({ move }) -> Move.to_str(move) != "e1g1"
        _ -> Bool.false
# Black has already been checkmated
expect
    board = FenParser.fen_to_board(Fen.black_is_check_mated)
    when find_move(board, [], Black) is
        FoundMove(_) -> Bool.false
        Mated -> Bool.true
        Draw -> Bool.false

## Return the best move (and score) for the side to move. The best move is the
## move with the highest score for the side to move. It will be positive if the
## side to move is in the lead. If the move is unknown, e.g. because there were
## no legal moves, this function returns move 0, which is not a legal move.
##
## Note: This function expects to be called with a live position, and not a
## position that is already checkmate or forced draw.
alpha_beta : Board, List Board, Color, U64, I64, I64 -> { move : Move, score : I64 }
alpha_beta = |board, board_history, side_to_move, depth, alpha, beta|
    if depth == 0 then
        # Depth = 0 -> evaluate current position for the side that moved,
        # and negate the score, because alphaBeta returns a positive score
        # if the side to move is in the lead
        { move: 0, score: -(Evaluator.evaluate(board, Color.flip_color(side_to_move))) }
    else
        # _ = dbgAlphaBeta depth sideToMove alpha beta
        # Depth > 0 -> generate moves and call alphaBeta recursively
        # Generate all pseudo legal moves
        moves = if side_to_move == White then board.white_moves else board.black_moves
        # Sort moves by their potential to make the alpha-beta algorithm more efficient
        sorted_moves = if depth >= 3 then List.sort_with(moves, compare_by_potential) else moves

        # Initialize best score to alpha
        best = List.walk_until(
            sorted_moves,
            { move: 0, score: alpha },
            |state, move|
                # _ = if depth != 1 then dbgBeforeCall depth sideToMove move else 0
                new_board = make_move(board, move, side_to_move)
                new_board_history = List.append(board_history, new_board)

                when is_end_of_game_position(new_board, new_board_history, side_to_move) is
                    Yes({ score }) ->
                        if score > state.score then Continue({ move: 0, score: score }) else Continue(state)

                    No ->
                        # Call recursively after flipping the side to move, and decreasing the depth
                        # Use best score so far as alpha
                        { score: s } = alpha_beta(new_board, new_board_history, Color.flip_color(side_to_move), (depth - 1), -beta, -state.score)
                        # The score returned from alphaBeta is the score for the best move
                        # for the side to move when calling alphaBeta, that is, not for our
                        # side. Thus we have to negate the score returned from alphaBeta.
                        score = -s
                        # _ = dbgAfterCall depth sideToMove move m score
                        if score > beta then
                            # If the score is too good, we cut off the search tree here,
                            # because the opponent will not select this branch
                            # _ = dbgBetaCutOff depth sideToMove move score beta
                            Break({ move, score: beta })
                        else if score > state.score or (score == state.score and state.move == 0) then
                            # If this was the best move yet, save move and score in state as the new alpha
                            if score == checkmate_value then
                                # If the best move is checkmate we don't have to look further
                                Break({ move, score })
                            else
                                Continue({ move, score })
                        else
                            # Default case: not the best move yet
                            Continue(state),
        )
        # _ = dbgBestMove depth sideToMove best

        # If there are no moves, and we are in check, we have been mated
        # If there are no moves, and we are not in check, it is a draw
        # If we found no legal move, and we are in check, we have been mated
        # If we found no legal move, and we are not in check, there is a draw
        if List.len(moves) == 0 then
            mate_or_stalemate(board, side_to_move)
        else if best.move == 0 then
            mate_or_stalemate(board, side_to_move)
        else
            best

## Check if the current end-of-game position is a mate or stalemate.
mate_or_stalemate : Board, Color -> { move : Move, score : I64 }
mate_or_stalemate = |board, side_to_move|
    if Checker.is_check(board, side_to_move) then
        # Mate
        { move: 0, score: illegal_check_value }
    else
        # Stalemate
        { move: 0, score: draw_value }

# Depth 0
expect
    board = Util.with_moves(initial_board, ["e2e4", "d7d5", "e4d5"], White)
    board_with_moves = MoveGenerator.with_moves(board)
    { score } = alpha_beta(board_with_moves, [], Black, 0, -initial_beta, -initial_alpha)
    # Black is a pawn down (just evaluation since depth is 0)
    score < 0
expect
    board = FenParser.fen_to_board(Fen.fools_mate)
    board_with_moves = MoveGenerator.with_moves(board)
    { score } = alpha_beta(board_with_moves, [], White, 0, -initial_beta, -initial_alpha)
    # White is mated, but we only know that he is checked
    score < 0
expect
    board = FenParser.fen_to_board(Fen.scholars_mate)
    board_with_moves = MoveGenerator.with_moves(board)
    { score } = alpha_beta(board_with_moves, [], Black, 0, -initial_beta, -initial_alpha)
    # Black is mated, but we only know he is checked
    score < 0

# Depth 1
expect
    { board, history } = Util.with_history(initial_board, ["e2e4"], White)
    board_with_moves = MoveGenerator.with_moves(board)
    { score } = alpha_beta(board_with_moves, history, Black, 1, -initial_beta, -initial_alpha)
    # Score should be equal after e.g. 1. e4 e5
    score == 0
expect
    { board, history } = Util.with_history(initial_board, ["e2e4", "d7d5"], White)
    board_with_moves = MoveGenerator.with_moves(board)
    { score } = alpha_beta(board_with_moves, history, White, 1, -initial_beta, -initial_alpha)
    # White is a pawn up after 2. exd5
    score >= 1_000
expect
    { board, history } = Util.with_history(initial_board, ["d2d4", "e7e5"], White)
    board_with_moves = MoveGenerator.with_moves(board)
    { score } = alpha_beta(board_with_moves, history, White, 1, -initial_beta, -initial_alpha)
    # White is a pawn up after 2. dxe5
    score >= 1_000
expect
    board = FenParser.fen_to_board(Fen.fools_mate_in_one)
    board_with_moves = MoveGenerator.with_moves(board)
    { score } = alpha_beta(board_with_moves, [], Black, 1, -initial_beta, -initial_alpha)
    # White is mated after 2... Qh4#
    # We don't know that, because evaluate does not test for checkmate
    # But we know White is checked, which gives a positive score for Black
    score > 0
expect
    board = FenParser.fen_to_board(Fen.draw_by_stale_mate)
    board_with_moves = MoveGenerator.with_moves(board)
    { score } = alpha_beta(board_with_moves, [], Black, 1, -initial_beta, -initial_alpha)
    score == 0

# Depth 2
expect
    { board, history } = Util.with_history(initial_board, ["e2e4", "d7d5"], White)
    board_with_moves = MoveGenerator.with_moves(board)
    { move, score } = alpha_beta(board_with_moves, history, White, 2, -initial_beta, -initial_alpha)
    # White is not a pawn up after 2. exd5 because black recaptures 2... Qxd5
    # Therefore White chooses another move that increases mobility
    score > 0 and Move.to_str(move) != "e4d5"
expect
    { board, history } = Util.with_history(initial_board, ["d2d4", "e7e5"], White)
    board_with_moves = MoveGenerator.with_moves(board)
    { move, score } = alpha_beta(board_with_moves, history, White, 2, -initial_beta, -initial_alpha)
    # White is a pawn up after 2. dxe5
    score > 0 and Move.to_str(move) == "d4e5"
expect
    board = FenParser.fen_to_board(Fen.fools_mate_in_one)
    board_with_moves = MoveGenerator.with_moves(board)
    { move, score } = alpha_beta(board_with_moves, [], Black, 2, -initial_beta, -initial_alpha)
    # White is mated after 2... Qh4#
    score == checkmate_value and Move.to_str(move) == "d8h4"

# Depth 3
expect
    board = FenParser.fen_to_board(Fen.fools_mate_in_one)
    board_with_moves = MoveGenerator.with_moves(board)
    { move, score } = alpha_beta(board_with_moves, [], Black, 3, -initial_beta, -initial_alpha)
    # White is mated
    score == checkmate_value and Move.to_str(move) == "d8h4"
expect
    board = FenParser.fen_to_board(Fen.scholars_mate_in_one)
    board_with_moves = MoveGenerator.with_moves(board)
    { move, score } = alpha_beta(board_with_moves, [], White, 3, -initial_beta, -initial_alpha)
    # Black is mated
    score == checkmate_value and Move.to_str(move) == "h5f7"
expect
    board = FenParser.fen_to_board(Fen.draw_by50_move_rule_in_one)
    board_with_moves = MoveGenerator.with_moves(board)
    { move, score } = alpha_beta(board_with_moves, [], White, 3, -initial_beta, -initial_alpha)
    # White is up by a rook after e6e7, which is the only move not resulting in a forced draw
    score > 0 and Move.to_str(move) == "e6e7"
expect
    board = FenParser.fen_to_board(Fen.draw_by_stale_mate)
    board_with_moves = MoveGenerator.with_moves(board)
    { score } = alpha_beta(board_with_moves, [], Black, 3, -initial_beta, -initial_alpha)
    score == 0
expect
    board = FenParser.fen_to_board(Fen.draw_by_stale_mate_in_one)
    board_with_moves = MoveGenerator.with_moves(board)
    { score, move } = alpha_beta(board_with_moves, [], White, 3, -initial_beta, -initial_alpha)
    score > 0 and Move.to_str(move) != "a1b1"
expect
    board = FenParser.fen_to_board(Fen.draw_by_stale_mate_is_best)
    board_with_moves = MoveGenerator.with_moves(board)
    { score, move } = alpha_beta(board_with_moves, [], White, 3, -initial_beta, -initial_alpha)
    score == 0 and Move.to_str(move) == "a2b2"
expect
    board = FenParser.fen_to_board(Fen.back_rank_mate_in_three)
    board_with_moves = MoveGenerator.with_moves(board)
    { score, move } = alpha_beta(board_with_moves, [], Black, 3, -initial_beta, -initial_alpha)
    score > 0 and Move.to_str(move) == "a8a1"

## Check if the given board represents an end-of-game position, such as a forced draw.
## Return Yes with the score if true. Return No otherwise.
is_end_of_game_position : Board, List Board, Color -> [Yes { score : I64 }, No]
is_end_of_game_position = |board, board_history, side_to_move|
    if Checker.is_check(board, side_to_move) then
        # The side to move is in check after the move
        Yes({ score: illegal_check_value })
    else if Checker.is_draw_by50_move_rule(board) then
        # Forced draw after the move
        Yes({ score: draw_value })
    else if Checker.is_draw_by_threefold_repetition(board, board_history) then
        # Forced draw after the move
        Yes({ score: draw_value })
    else
        No

expect
    board = FenParser.fen_to_board(Fen.initial_game)
    board_with_moves = MoveGenerator.with_moves(board)
    is_end_of_game_position(board_with_moves, [], White) == No
expect
    board = FenParser.fen_to_board(Fen.scholars_mate)
    board_with_moves = MoveGenerator.with_moves(board)
    is_end_of_game_position(board_with_moves, [], Black) == Yes({ score: illegal_check_value })
expect
    board =
        FenParser.fen_to_board(Fen.initial_game)
        |> |b| { b & flags: 100 }
    board_with_moves = MoveGenerator.with_moves(board)
    is_end_of_game_position(board_with_moves, [], White) == Yes({ score: draw_value })
expect
    board = FenParser.fen_to_board(Fen.initial_game)
    board_with_moves = MoveGenerator.with_moves(board)
    is_end_of_game_position(board_with_moves, [board, board, board], White) == Yes({ score: draw_value })

## Compare moves by their score. The move with the highest score will be sorted first.
## Not used anymore. Remove?
compare_by_score : { move : Move, score : I64 }, { move : Move, score : I64 } -> [LT, EQ, GT]
compare_by_score = |m1, m2|
    if m1.score > m2.score then
        LT
    else if m2.score > m1.score then
        GT
    else
        EQ

expect compare_by_score({ move: 0, score: 7 }, { move: 0, score: 5 }) == LT
expect compare_by_score({ move: 0, score: 5 }, { move: 0, score: 5 }) == EQ
expect compare_by_score({ move: 0, score: 3 }, { move: 0, score: 5 }) == GT

## Compare by potential. Because of how the bits in a Move are organized,
## captures will be sorted before promotions that will be sorted before
## ordinary moves.
compare_by_potential : Move, Move -> [LT, EQ, GT]
compare_by_potential = |m1, m2|
    if m1 > m2 then
        LT
    else if m2 > m1 then
        GT
    else
        EQ

## Make the given move, and decorate the resulting board
## with the possible moves on that board.
make_move : Board, Move, Color -> Board
make_move = |board, move, side_to_move|
    new_board = Board.make_move(board, move, side_to_move)
    MoveGenerator.with_moves(new_board)

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

# dbgBeforeCall = \depth, sideToMove, move ->
#    m = "$(Num.toStr depth): Checking $(Inspect.toStr sideToMove) move $(Move.toStr move)"
#    dbg m

#    0

# dbgAfterCall = \depth, sideToMove, myMove, theirMove, myScore ->
#    m =
#        if theirMove == 0 then
#            "$(Num.toStr depth): If $(Inspect.toStr sideToMove) plays $(Move.toStr myMove) the score for $(Inspect.toStr sideToMove) is $(Num.toStr myScore)"
#        else
#            "$(Num.toStr depth): If $(Inspect.toStr sideToMove) plays $(Move.toStr myMove) then $(Inspect.toStr (Color.flipColor sideToMove)) plays $(Move.toStr theirMove) and the score for $(Inspect.toStr sideToMove) is $(Num.toStr myScore)"
#    dbg m

#    0

# dbgBestMove = \depth, sideToMove, { move, score } ->
#    m = "$(Num.toStr depth): Best move for $(Inspect.toStr sideToMove) is $(Move.toStr move) with score $(Num.toStr score)"
#    dbg m

#    0

# dbgAlphaBeta = \depth, sideToMove, alpha, beta ->
#    m = "$(Num.toStr depth): Side to move is $(Inspect.toStr sideToMove), alpha is $(Num.toStr alpha), beta is $(Num.toStr beta)"
#    dbg m

#    0

# dbgBetaCutOff = \depth, sideToMove, move, score, beta ->
#    m = "$(Num.toStr depth): Beta cut-off on $(Inspect.toStr sideToMove) move $(Move.toStr move) with score $(Num.toStr score) > beta $(Num.toStr beta)"
#    dbg m

#    0
