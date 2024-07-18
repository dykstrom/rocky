module [findMove]

import Board exposing [Board, initialBoard]
import Checker
import Color exposing [Color]
import Evaluator exposing [illegalCheckValue, checkmateValue, drawValue]
import Fen
import FenParser
import MoveGenerator
import Move exposing [Move]
import Util

initialAlpha = -3_000_000
initialBeta = 3_000_000

## Find the best move for the side to move. The best move is the
## move with the highest score for the side to move. It will be
## positive if the side to move is in the lead.
findMove : Board, List Board, Color -> [Draw, Mated, FoundMove { move : Move, score : I64 }]
findMove = \board, boardHistory, sideToMove ->
    # Decorate the initial board with the possible moves
    boardWithMoves = MoveGenerator.withMoves board
    { move, score } = alphaBeta boardWithMoves boardHistory sideToMove 3 -initialBeta -initialAlpha
    if move == 0 then
        if Checker.isCheck board sideToMove then Mated else Draw
    else
        FoundMove { move, score }

# White captures 2. dxe5
expect
    { board, history } = Util.withHistory initialBoard ["d2d4", "e7e5"] White
    when findMove board history White is
        FoundMove { move } -> Move.toStr move == "d4e5"
        _ -> Bool.false
# Fool's mate in one
expect
    board = FenParser.fenToBoard Fen.foolsMateInOne
    when findMove board [] Black is
        FoundMove { move } -> Move.toStr move == "d8h4"
        _ -> Bool.false

## Return the best move (and score) for the side to move. The best move is the
## move with the highest score for the side to move. It will be positive if the
## side to move is in the lead. If the move is unknown, e.g. because there were
## no legal moves, this function returns move 0, which is not a legal move.
##
## Note: This function expects to be called with a live position, and not a
## position that is already checkmate or forced draw.
alphaBeta : Board, List Board, Color, U64, I64, I64 -> { move : Move, score : I64 }
alphaBeta = \board, boardHistory, sideToMove, depth, alpha, beta ->
    if depth == 0 then
        # Depth = 0 -> evaluate current position for the side that moved,
        # and negate the score, because alphaBeta returns a positive score
        # if the side to move is in the lead
        { move: 0, score: -(Evaluator.evaluate board (Color.flipColor sideToMove)) }
    else
        # _ = dbgAlphaBeta depth sideToMove alpha beta
        # Depth > 0 -> generate moves and call alphaBeta recursively
        # Generate all pseudo legal moves
        moves = if sideToMove == White then board.whiteMoves else board.blackMoves
        # Sort moves by their potential to make the alpha-beta algorithm more efficient
        sortedMoves = if depth >= 3 then List.sortWith moves compareByPotential else moves

        # Initialize best score to alpha
        best = List.walkUntil sortedMoves { move: 0, score: alpha } \state, move ->
            # _ = if depth != 1 then dbgBeforeCall depth sideToMove move else 0
            newBoard = makeMove board move sideToMove
            newBoardHistory = List.append boardHistory newBoard

            when isEndOfGamePosition newBoard newBoardHistory sideToMove is
                Yes { score } ->
                    if score > state.score then Continue { move: 0, score: score } else Continue state

                No ->
                    # Call recursively after flipping the side to move, and decreasing the depth
                    # Use best score so far as alpha
                    { score: s } = alphaBeta newBoard newBoardHistory (Color.flipColor sideToMove) (depth - 1) -beta -state.score
                    # The score returned from alphaBeta is the score for the best move
                    # for the side to move when calling alphaBeta, that is, not for our
                    # side. Thus we have to negate the score returned from alphaBeta.
                    score = -s
                    # _ = dbgAfterCall depth sideToMove move m score
                    if score > beta then
                        # If the score is too good, we cut off the search tree here,
                        # because the opponent will not select this branch
                        # _ = dbgBetaCutOff depth sideToMove move score beta
                        Break { move, score: beta }
                    else if score > state.score then
                        # If this was the best move yet, save move and score in state as the new alpha
                        if score == checkmateValue then
                            # If the best move is checkmate we don't have to look further
                            Break { move, score }
                        else
                            Continue { move, score }
                    else
                        # Default case: not the best move yet
                        Continue state
        # _ = dbgBestMove depth sideToMove best

        # If there are no moves, and we are in check, we have been mated
        # If there are no moves, and we are not in check, it is a draw
        # If the best move is illegalCheckValue, and we are in check, we have been mated
        # If the best move is illegalCheckValue, and we are not in check, there is a draw
        if List.len moves == 0 then
            mateOrStalemate board sideToMove
        else if best.score == illegalCheckValue then
            mateOrStalemate board sideToMove
        else
            best

## Check if the current end-of-game position is a mate or stalemate.
mateOrStalemate : Board, Color -> { move : Move, score : I64 }
mateOrStalemate = \board, sideToMove ->
    if Checker.isCheck board sideToMove then
        # Mate
        { move: 0, score: illegalCheckValue }
    else
        # Stalemate
        { move: 0, score: drawValue }

# Depth 0
expect
    board = Util.withMoves initialBoard ["e2e4", "d7d5", "e4d5"] White
    boardWithMoves = MoveGenerator.withMoves board
    { score } = alphaBeta boardWithMoves [] Black 0 -initialBeta -initialAlpha
    # Black is a pawn down (just evaluation since depth is 0)
    score < 0
expect
    board = FenParser.fenToBoard Fen.foolsMate
    boardWithMoves = MoveGenerator.withMoves board
    { score } = alphaBeta boardWithMoves [] White 0 -initialBeta -initialAlpha
    # White is mated, but we only know that he is checked
    score < 0
expect
    board = FenParser.fenToBoard Fen.scholarsMate
    boardWithMoves = MoveGenerator.withMoves board
    { score } = alphaBeta boardWithMoves [] Black 0 -initialBeta -initialAlpha
    # Black is mated, but we only know he is checked
    score < 0

# Depth 1
expect
    { board, history } = Util.withHistory initialBoard ["e2e4"] White
    boardWithMoves = MoveGenerator.withMoves board
    { score } = alphaBeta boardWithMoves history Black 1 -initialBeta -initialAlpha
    # Score should be equal after e.g. 1. e4 e5
    score == 0
expect
    { board, history } = Util.withHistory initialBoard ["e2e4", "d7d5"] White
    boardWithMoves = MoveGenerator.withMoves board
    { score } = alphaBeta boardWithMoves history White 1 -initialBeta -initialAlpha
    # White is a pawn up after 2. exd5
    score >= 1_000
expect
    { board, history } = Util.withHistory initialBoard ["d2d4", "e7e5"] White
    boardWithMoves = MoveGenerator.withMoves board
    { score } = alphaBeta boardWithMoves history White 1 -initialBeta -initialAlpha
    # White is a pawn up after 2. dxe5
    score >= 1_000
expect
    board = FenParser.fenToBoard Fen.foolsMateInOne
    boardWithMoves = MoveGenerator.withMoves board
    { score } = alphaBeta boardWithMoves [] Black 1 -initialBeta -initialAlpha
    # White is mated after 2... Qh4#
    # We don't know that, because evaluate does not test for checkmate
    # But we know White is checked, which gives a positive score for Black
    score > 0
expect
    board = FenParser.fenToBoard Fen.drawByStaleMate
    boardWithMoves = MoveGenerator.withMoves board
    { score } = alphaBeta boardWithMoves [] Black 1 -initialBeta -initialAlpha
    score == 0

# Depth 2
expect
    { board, history } = Util.withHistory initialBoard ["e2e4", "d7d5"] White
    boardWithMoves = MoveGenerator.withMoves board
    { move, score } = alphaBeta boardWithMoves history White 2 -initialBeta -initialAlpha
    # White is not a pawn up after 2. exd5 because black recaptures 2... Qxd5
    # Therefore White chooses another move that increases mobility
    score > 0 && Move.toStr move != "e4d5"
expect
    { board, history } = Util.withHistory initialBoard ["d2d4", "e7e5"] White
    boardWithMoves = MoveGenerator.withMoves board
    { move, score } = alphaBeta boardWithMoves history White 2 -initialBeta -initialAlpha
    # White is a pawn up after 2. dxe5
    score > 0 && Move.toStr move == "d4e5"
expect
    board = FenParser.fenToBoard Fen.foolsMateInOne
    boardWithMoves = MoveGenerator.withMoves board
    { move, score } = alphaBeta boardWithMoves [] Black 2 -initialBeta -initialAlpha
    # White is mated after 2... Qh4#
    score == checkmateValue && Move.toStr move == "d8h4"

# Depth 3
expect
    board = FenParser.fenToBoard Fen.foolsMateInOne
    boardWithMoves = MoveGenerator.withMoves board
    { move, score } = alphaBeta boardWithMoves [] Black 3 -initialBeta -initialAlpha
    # White is mated
    score == checkmateValue && Move.toStr move == "d8h4"
expect
    board = FenParser.fenToBoard Fen.scholarsMateInOne
    boardWithMoves = MoveGenerator.withMoves board
    { move, score } = alphaBeta boardWithMoves [] White 3 -initialBeta -initialAlpha
    # Black is mated
    score == checkmateValue && Move.toStr move == "h5f7"
expect
    board = FenParser.fenToBoard Fen.drawBy50MoveRuleInOne
    boardWithMoves = MoveGenerator.withMoves board
    { move, score } = alphaBeta boardWithMoves [] White 3 -initialBeta -initialAlpha
    # White is up by a rook after e6e7, which is the only move not resulting in a forced draw
    score > 0 && Move.toStr move == "e6e7"
expect
    board = FenParser.fenToBoard Fen.drawByStaleMate
    boardWithMoves = MoveGenerator.withMoves board
    { score } = alphaBeta boardWithMoves [] Black 3 -initialBeta -initialAlpha
    score == 0
expect
    board = FenParser.fenToBoard Fen.drawByStaleMateInOne
    boardWithMoves = MoveGenerator.withMoves board
    { score, move } = alphaBeta boardWithMoves [] White 3 -initialBeta -initialAlpha
    score > 0 && Move.toStr move != "a1b1"
expect
    board = FenParser.fenToBoard Fen.drawByStaleMateIsBest
    boardWithMoves = MoveGenerator.withMoves board
    { score, move } = alphaBeta boardWithMoves [] White 3 -initialBeta -initialAlpha
    score == 0 && Move.toStr move == "a2b2"
expect
    board = FenParser.fenToBoard Fen.backRankMateInThree
    boardWithMoves = MoveGenerator.withMoves board
    { score, move } = alphaBeta boardWithMoves [] Black 3 -initialBeta -initialAlpha
    score > 0 && Move.toStr move == "a8a1"

## Check if the given board represents an end-of-game position, such as a forced draw.
## Return Yes with the score if true. Return No otherwise.
isEndOfGamePosition : Board, List Board, Color -> [Yes { score : I64 }, No]
isEndOfGamePosition = \board, boardHistory, sideToMove ->
    if Checker.isCheck board sideToMove then
        # The side to move is in check after the move
        Yes { score: illegalCheckValue }
    else if Checker.isDrawBy50MoveRule board then
        # Forced draw after the move
        Yes { score: drawValue }
    else if Checker.isDrawByThreefoldRepetition board boardHistory then
        # Forced draw after the move
        Yes { score: drawValue }
    else
        No

expect
    board = FenParser.fenToBoard Fen.initialGame
    boardWithMoves = MoveGenerator.withMoves board
    isEndOfGamePosition boardWithMoves [] White == No
expect
    board = FenParser.fenToBoard Fen.scholarsMate
    boardWithMoves = MoveGenerator.withMoves board
    isEndOfGamePosition boardWithMoves [] Black == Yes { score: illegalCheckValue }
expect
    board =
        FenParser.fenToBoard Fen.initialGame
        |> \b -> { b & flags: 100 }
    boardWithMoves = MoveGenerator.withMoves board
    isEndOfGamePosition boardWithMoves [] White == Yes { score: drawValue }
expect
    board = FenParser.fenToBoard Fen.initialGame
    boardWithMoves = MoveGenerator.withMoves board
    isEndOfGamePosition boardWithMoves [board, board, board] White == Yes { score: drawValue }

## Compare moves by their score. The move with the highest score will be sorted first.
## Not used anymore. Remove?
compareByScore : { move : Move, score : I64 }, { move : Move, score : I64 } -> [LT, EQ, GT]
compareByScore = \m1, m2 ->
    if m1.score > m2.score then
        LT
    else if m2.score > m1.score then
        GT
    else
        EQ

expect compareByScore { move: 0, score: 7 } { move: 0, score: 5 } == LT
expect compareByScore { move: 0, score: 5 } { move: 0, score: 5 } == EQ
expect compareByScore { move: 0, score: 3 } { move: 0, score: 5 } == GT

## Compare by potential. Because of how the bits in a Move are organized,
## captures will be sorted before promotions that will be sorted before
## ordinary moves.
compareByPotential : Move, Move -> [LT, EQ, GT]
compareByPotential = \m1, m2 ->
    if m1 > m2 then
        LT
    else if m2 > m1 then
        GT
    else
        EQ

## Make the given move, and decorate the resulting board
## with the possible moves on that board.
makeMove : Board, Move, Color -> Board
makeMove = \board, move, sideToMove ->
    newBoard = Board.makeMove board move sideToMove
    MoveGenerator.withMoves newBoard

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
