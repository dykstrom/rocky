module [Game, initialGame, makeMove, unmakeMove]

import Board exposing [Board, initialBoard]
import Color exposing [Color]
import Move exposing [Move]
import Time exposing [TimeControl, initialTimeControl]

Game : {
    activeColor : Color,
    engineColor : Color,
    forceMode : [On, Off],
    moveHistory : List Move,
    moveNumber : U64,
    boardHistory : List Board,
    board : Board,
    debug : [On, Off],
    pretty : [On, Off],
    timeControl : TimeControl,
    timeLeft : I128,
    movesLeft : I128,
}

initialGame : Game
initialGame = {
    activeColor: White,
    engineColor: Black,
    forceMode: Off,
    moveHistory: [],
    moveNumber: 1,
    boardHistory: [],
    board: initialBoard,
    debug: Off,
    pretty: On,
    timeControl: initialTimeControl,
    timeLeft: Num.toI128 initialTimeControl.base,
    movesLeft: initialTimeControl.moves,
}

makeMove : Game, Move -> Game
makeMove = \game, move ->
    newBoardHistory = List.append game.boardHistory game.board
    newBoard = Board.makeMove game.board move game.activeColor
    newColor = Color.flipColor game.activeColor
    newMoveNumber = game.moveNumber + if newColor == White then 1 else 0
    newMoveHistory = List.append game.moveHistory move
    { game &
        activeColor: newColor,
        moveNumber: newMoveNumber,
        moveHistory: newMoveHistory,
        board: newBoard,
        boardHistory: newBoardHistory,
    }

unmakeMove : Game -> Game
unmakeMove = \game ->
    newColor = Color.flipColor game.activeColor
    newMoveNumber = game.moveNumber - if newColor == Black then 1 else 0
    newMoveHistory = List.dropLast game.moveHistory 1
    newBoard =
        when List.last game.boardHistory is
            Ok board -> board
            Err ListWasEmpty -> crash "Should not happen: boardHistory is empty"
    newBoardHistory = List.dropLast game.boardHistory 1
    { game &
        activeColor: newColor,
        moveNumber: newMoveNumber,
        moveHistory: newMoveHistory,
        board: newBoard,
        boardHistory: newBoardHistory,
    }
