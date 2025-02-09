module [Game, initial_game, make_move, unmake_move]

import Board exposing [Board, initial_board]
import Color exposing [Color]
import Move exposing [Move]
import Time exposing [TimeControl, initial_time_control]

Game : {
    active_color : Color,
    engine_color : Color,
    force_mode : [On, Off],
    move_history : List Move,
    move_number : U64,
    board_history : List Board,
    board : Board,
    debug : [On, Off],
    pretty : [On, Off],
    time_control : TimeControl,
    time_left : I128,
    moves_left : I128,
}

initial_game : Game
initial_game = {
    active_color: White,
    engine_color: Black,
    force_mode: Off,
    move_history: [],
    move_number: 1,
    board_history: [],
    board: initial_board,
    debug: Off,
    pretty: On,
    time_control: initial_time_control,
    time_left: Num.to_i128(initial_time_control.base),
    moves_left: initial_time_control.moves,
}

make_move : Game, Move -> Game
make_move = |game, move|
    new_board_history = List.append(game.board_history, game.board)
    new_board = Board.make_move(game.board, move, game.active_color)
    new_color = Color.flip_color(game.active_color)
    new_move_number = game.move_number + if new_color == White then 1 else 0
    new_move_history = List.append(game.move_history, move)
    { game &
        active_color: new_color,
        move_number: new_move_number,
        move_history: new_move_history,
        board: new_board,
        board_history: new_board_history,
    }

unmake_move : Game -> Game
unmake_move = |game|
    new_color = Color.flip_color(game.active_color)
    new_move_number = game.move_number - if new_color == Black then 1 else 0
    new_move_history = List.drop_last(game.move_history, 1)
    new_board =
        when List.last(game.board_history) is
            Ok(board) -> board
            Err(ListWasEmpty) -> crash("Should not happen: boardHistory is empty")
    new_board_history = List.drop_last(game.board_history, 1)
    { game &
        active_color: new_color,
        move_number: new_move_number,
        move_history: new_move_history,
        board: new_board,
        board_history: new_board_history,
    }
