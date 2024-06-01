module [Game, initialGame, makeMove, unmakeMove]

import Board exposing [Board, initialBoard]
import Move exposing [Move]
import Color exposing [Color]

Game : {
    activeColor : Color,
    engineColor : Color,
    forceMode : [On, Off],
    moveHistory : List Move,
    moveNumber : U64,
    boardHistory : List Board,
    board : Board,
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
