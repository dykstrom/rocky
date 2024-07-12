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
    { move, score } = alphaBeta board boardHistory sideToMove 3 -initialBeta -initialAlpha
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
        # Depth > 0 -> generate moves and call alphaBeta recursively
        # Generate all pseudo legal moves
        moves = MoveGenerator.generateMoves board sideToMove

        scoredMoves = List.map moves \move ->
            # _ = if depth != 1 then dbgBeforeCall depth sideToMove move else 0
            newBoard = Board.makeMove board move sideToMove
            newBoardHistory = List.append boardHistory newBoard

            when isEndOfGamePosition newBoard newBoardHistory sideToMove is
                Yes { score } ->
                    { move: move, score: score }

                No ->
                    # Call recursively after flipping the side to move, and decreasing the depth
                    { score } = alphaBeta newBoard newBoardHistory (Color.flipColor sideToMove) (depth - 1) -beta -alpha
                    # _ = dbgAfterCall depth sideToMove move m -score
                    # The score returned from alphaBeta is the score for the best move
                    # for the side to move when calling alphaBeta, that is, not for our
                    # side. Thus we have to negate the score returned from alphaBeta.
                    { move: move, score: -score }

        # Sort them according to score
        sortedMoves = List.sortWith scoredMoves compareByScore
        # _ = dbgMoves depth sideToMove sortedMoves

        # If the best move is illegalCheckValue we have been mated, or there is a draw
        # If there are no moves, and we are in check, we have been mated
        # If there are no moves, and we are not in check, it is a draw
        when List.get sortedMoves 0 is
            Ok { move, score } ->
                if score == illegalCheckValue then
                    mateOrStalemate board sideToMove
                else
                    { move, score }

            Err OutOfBounds -> mateOrStalemate board sideToMove

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
    { board, history } = Util.withHistory initialBoard ["e2e4", "d7d5", "e4d5"] White
    { score } = alphaBeta board history Black 0 -initialBeta -initialAlpha
    # Black is a pawn down (just evaluation since depth is 0)
    score < 0
expect
    board = FenParser.fenToBoard Fen.foolsMate
    { score } = alphaBeta board [] White 0 -initialBeta -initialAlpha
    # White is mated, but we only know that he is checked
    score < 0
expect
    board = FenParser.fenToBoard Fen.scholarsMate
    { score } = alphaBeta board [] Black 0 -initialBeta -initialAlpha
    # Black is mated, but we only know he is checked
    score < 0

# Depth 1
expect
    { board, history } = Util.withHistory initialBoard ["e2e4"] White
    { score } = alphaBeta board history Black 1 -initialBeta -initialAlpha
    # Score should be equal after e.g. 1. e4 e5
    score == 0
expect
    { board, history } = Util.withHistory initialBoard ["e2e4", "d7d5"] White
    { score } = alphaBeta board history White 1 -initialBeta -initialAlpha
    # White is a pawn up after 2. exd5
    score >= 1_000
expect
    { board, history } = Util.withHistory initialBoard ["d2d4", "e7e5"] White
    { score } = alphaBeta board history White 1 -initialBeta -initialAlpha
    # White is a pawn up after 2. dxe5
    score >= 1_000
expect
    board = FenParser.fenToBoard Fen.foolsMateInOne
    { score } = alphaBeta board [] Black 1 -initialBeta -initialAlpha
    # White is mated after 2... Qh4#
    # We don't know that, because evaluate does not test for checkmate
    # But we know White is checked, which gives a positive score for Black
    score > 0
expect
    board = FenParser.fenToBoard Fen.drawByStaleMate
    { score } = alphaBeta board [] Black 1 -initialBeta -initialAlpha
    score == 0

# Depth 2
expect
    { board, history } = Util.withHistory initialBoard ["e2e4", "d7d5"] White
    { move, score } = alphaBeta board history White 2 -initialBeta -initialAlpha
    # White is not a pawn up after 2. exd5 because black recaptures 2... Qxd5
    # Therefore White chooses another move that increases mobility
    score > 0 && Move.toStr move != "e4d5"
expect
    { board, history } = Util.withHistory initialBoard ["d2d4", "e7e5"] White
    { move, score } = alphaBeta board history White 2 -initialBeta -initialAlpha
    # White is a pawn up after 2. dxe5
    score > 0 && Move.toStr move == "d4e5"
expect
    board = FenParser.fenToBoard Fen.foolsMateInOne
    { move, score } = alphaBeta board [] Black 2 -initialBeta -initialAlpha
    # White is mated after 2... Qh4#
    score == checkmateValue && Move.toStr move == "d8h4"

# Depth 3
expect
    board = FenParser.fenToBoard Fen.foolsMateInOne
    { move, score } = alphaBeta board [] Black 3 -initialBeta -initialAlpha
    # White is mated
    score == checkmateValue && Move.toStr move == "d8h4"
expect
    board = FenParser.fenToBoard Fen.scholarsMateInOne
    { move, score } = alphaBeta board [] White 3 -initialBeta -initialAlpha
    # Black is mated
    score == checkmateValue && Move.toStr move == "h5f7"
expect
    board = FenParser.fenToBoard Fen.drawBy50MoveRuleInOne
    { move, score } = alphaBeta board [] White 3 -initialBeta -initialAlpha
    # White is up by a rook after e6e7, which is the only move not resulting in a forced draw
    score > 0 && Move.toStr move == "e6e7"
expect
    board = FenParser.fenToBoard Fen.drawByStaleMate
    { score } = alphaBeta board [] Black 3 -initialBeta -initialAlpha
    score == 0
expect
    board = FenParser.fenToBoard Fen.drawByStaleMateInOne
    { score, move } = alphaBeta board [] White 3 -initialBeta -initialAlpha
    score > 0 && Move.toStr move != "a1b1"
expect
    board = FenParser.fenToBoard Fen.drawByStaleMateIsBest
    { score, move } = alphaBeta board [] White 3 -initialBeta -initialAlpha
    score == 0 && Move.toStr move == "a2b2"
expect
    board = FenParser.fenToBoard Fen.backRankMateInThree
    { score, move } = alphaBeta board [] Black 3 -initialBeta -initialAlpha
    score > 0 && Move.toStr move == "a8a1"

# TODO: Make use of alpha and beta to do cutoff.

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
    isEndOfGamePosition board [] White == No
expect
    board = FenParser.fenToBoard Fen.scholarsMate
    isEndOfGamePosition board [] Black == Yes { score: illegalCheckValue }
expect
    board =
        FenParser.fenToBoard Fen.initialGame
        |> \b -> { b & flags: 100 }
    isEndOfGamePosition board [] White == Yes { score: drawValue }
expect
    board = FenParser.fenToBoard Fen.initialGame
    isEndOfGamePosition board [board, board, board] White == Yes { score: drawValue }

## Compare moves by their score. The move with the highest score will be sorted first.
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

# dbgIsInCheck = \sideToMove ->
#    m = "$(Inspect.toStr sideToMove) is in check!"
#    dbg m

#    0

# dbgIsDraw = \_sideToMove ->
#    m = "It is forced draw!"
#    dbg m

#    0

# dbgMoves = \depth, sideToMove, list ->
#    moves =
#        List.sublist list { start: 0, len: 3 }
#        |> List.map \{ move, score } ->
#            { move: Move.toStr move, score: score }
#    m = "$(Num.toStr depth): Best moves for $(Inspect.toStr sideToMove) are $(Inspect.toStr moves)"
#    dbg m

#    0
