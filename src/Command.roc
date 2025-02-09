module [
    accepted_cmd,
    board_cmd,
    computer_cmd,
    force_cmd,
    go_cmd,
    help_cmd,
    level_cmd,
    new_cmd,
    otim_cmd,
    ping_cmd,
    plain_cmd,
    play_other_cmd,
    protover_cmd,
    rejected_cmd,
    remove_cmd,
    result_cmd,
    set_board_cmd,
    time_cmd,
    undo_cmd,
    usermove_cmd,
    xboard_cmd,
]

import Board exposing [initial_board]
import Color
import FenParser
import Finder
import Game exposing [Game, initial_game]
import Move exposing [Move]
import MoveParser
import Time exposing [initial_time_control, allocate_time_for_move]
import Util
import Version exposing [version]

# ----------------------------------------------------------------------------

accepted_cmd : Game, List Str -> Result (Game, Str) _
accepted_cmd = |game, args|
    when args is
        ["debug"] -> Ok(({ game & debug: On }, ""))
        _ -> Ok((game, ""))

expect
    accepted_cmd({ initial_game & debug: Off }, ["debug"])
    ==
    Ok(({ initial_game & debug: On }, ""))
expect
    accepted_cmd({ initial_game & debug: Off }, ["ping"])
    ==
    Ok(({ initial_game & debug: Off }, ""))

# ----------------------------------------------------------------------------

board_cmd : Game, List Str -> Result (Game, Str) _
board_cmd = |game, _args|
    Ok((game, if game.pretty == On then Board.to_pretty_str(game.board) else Board.to_str(game.board)))

# ----------------------------------------------------------------------------

computer_cmd : Game, List Str -> Result (Game, Str) _
computer_cmd = |game, _args|
    Ok((game, ""))

# ----------------------------------------------------------------------------

force_cmd : Game, List Str -> Result (Game, Str) _
force_cmd = |game, _args|
    Ok(({ game & force_mode: On }, ""))

expect
    force_cmd({ initial_game & force_mode: Off }, [])
    ==
    Ok(({ initial_game & force_mode: On }, ""))

# ----------------------------------------------------------------------------

go_cmd : Game, List Str -> Result (Game, Str) _
go_cmd = |game, _args|
    game_before_move = { game & force_mode: Off, engine_color: game.active_color }
    # Make engine move
    make_engine_move(game_before_move)

expect
    # Given
    original = { initial_game & force_mode: On, active_color: White }
    # When
    actual = run_test(go_cmd, original, [])
    # Then
    actual.force_mode == Off and actual.active_color == Black and actual.engine_color == White and actual.move_number == 1 and List.len(actual.move_history) == 1

# ----------------------------------------------------------------------------

help_text =
    """
    Available commands
    ------------------
    board     = show current position
    force     = turn force mode on
    go        = turn force mode off and set the chess engine to
                play the color that is on move
    help      = show this help text
    level     = set time control (see xboard documentation)
    new       = start a new game with the chess engine as black
    ping      = ping the chess engine
    plain     = show the board in plain text without any colors
    playother = turn force mode off and set the chess engine to
                play the color that is not on move
    quit      = quit program
    remove    = retract latest move pair, and let the user move again
    setboard  = set current position to the given FEN position
    usermove  = submit a move in coordinate algebraic notation
    xboard    = put the engine in xboard mode
    """

help_cmd : Game, List Str -> Result (Game, Str) _
help_cmd = |game, _args|
    Ok((game, help_text))

# ----------------------------------------------------------------------------

level_cmd : Game, List Str -> Result (Game, Str) _
level_cmd = |game, args|
    when args is
        [mps, base, inc] ->
            when Time.parse_time_control(mps, base, inc) is
                Ok(tc) -> Ok(({ game & time_control: tc, time_left: tc.base, moves_left: tc.moves }, ""))
                Err(error) -> Err(error)

        _ -> Err(SyntaxError)

expect
    level_cmd({ initial_game & time_control: initial_time_control, time_left: 0, moves_left: 0 }, ["40", "60", "0"])
    ==
    Ok(({ initial_game & time_control: { type: Classic, moves: 40, base: 3_600_000, inc: 0 }, time_left: 3_600_000, moves_left: 40 }, ""))

# ----------------------------------------------------------------------------

new_cmd : Game, List Str -> Result (Game, Str) _
new_cmd = |game, _args|
    # Keep feature and settings
    Ok(({ initial_game & debug: game.debug, pretty: game.pretty }, ""))

expect
    new_cmd({ initial_game & debug: On, pretty: Off }, [])
    ==
    Ok(({ initial_game & debug: On, pretty: Off }, ""))

# ----------------------------------------------------------------------------

otim_cmd : Game, List Str -> Result (Game, Str) _
otim_cmd = |game, _args|
    Ok((game, ""))

# ----------------------------------------------------------------------------

# TODO: Change all command to return a tuple of (Game, Str) like roc format almost
# changed it into.

ping_cmd : Game, List Str -> Result (Game, Str) _
ping_cmd = |game, args|
    when args is
        [arg] -> Ok((game, "pong ${arg}"))
        _ -> Err(SyntaxError)

expect ping_cmd(initial_game, ["1"]) == Ok((initial_game, "pong 1"))
expect ping_cmd(initial_game, []) == Err(SyntaxError)

# ----------------------------------------------------------------------------

plain_cmd : Game, List Str -> Result (Game, Str) _
plain_cmd = |game, _args|
    Ok(({ game & pretty: Off }, ""))

# ----------------------------------------------------------------------------

play_other_cmd : Game, List Str -> Result (Game, Str) _
play_other_cmd = |game, _args|
    Ok(({ game & force_mode: Off, engine_color: Color.flip_color(game.active_color) }, ""))

expect
    play_other_cmd({ initial_game & force_mode: On, active_color: Black, engine_color: Black }, [])
    ==
    Ok(({ initial_game & force_mode: Off, active_color: Black, engine_color: White }, ""))

# ----------------------------------------------------------------------------

protover_cmd : Game, List Str -> Result (Game, Str) _
protover_cmd = |game, _args|
    Ok((game, "feature ping=1\nfeature setboard=1\nfeature playother=1\nfeature san=0\nfeature usermove=1\nfeature time=1\nfeature draw=0\nfeature sigint=0\nfeature sigterm=0\nfeature reuse=1\nfeature analyze=0\nfeature myname=\"rocky ${Str.trim(version)}\"\nfeature variants=\"normal\"\nfeature colors=0\nfeature ics=0\nfeature name=0\nfeature pause=0\nfeature debug=1\nfeature done=1"))

# ----------------------------------------------------------------------------

rejected_cmd : Game, List Str -> Result (Game, Str) _
rejected_cmd = |game, _args|
    Ok((game, ""))

# ----------------------------------------------------------------------------

remove_cmd : Game, List Str -> Result (Game, Str) _
remove_cmd = |game, _args|
    if List.len(game.board_history) >= 2 then
        game_after_unmake = Game.unmake_move(game) |> Game.unmake_move
        Ok((game_after_unmake, ""))
    else
        Err(NotLegal)

expect
    # Given
    original = { initial_game & move_number: 2, move_history: [0, 0], board_history: [initial_board, initial_board] }
    # When
    actual = run_test(remove_cmd, original, [])
    # Then
    expected = { original & move_number: 1, move_history: [], board_history: [] }
    actual == expected

# ----------------------------------------------------------------------------

result_cmd : Game, List Str -> Result (Game, Str) _
result_cmd = |game, _args|
    Ok((game, ""))

# ----------------------------------------------------------------------------

set_board_cmd : Game, List Str -> Result (Game, Str) _
set_board_cmd = |game, args|
    g = FenParser.from_list(args)?
    Ok(
        (
            { g &
                debug: game.debug,
                pretty: game.pretty,
                time_control: game.time_control,
                time_left: game.time_control.base,
                moves_left: game.time_control.moves,
            },
            "",
        ),
    )

# ----------------------------------------------------------------------------

time_cmd : Game, List Str -> Result (Game, Str) _
time_cmd = |game, args|
    when args is
        [arg] ->
            when Str.to_i128(arg) is
                Ok(time_in_cs) ->
                    time_in_ms = time_in_cs * 10
                    # Save time left according to XBoard
                    Ok(
                        (
                            { game & time_left: time_in_ms },
                            Util.debug(game, "XBoard says time left is ${Time.format_time(time_in_ms)}"),
                        ),
                    )

                _ -> Err(SyntaxError)

        _ -> Err(SyntaxError)

# ----------------------------------------------------------------------------

undo_cmd : Game, List Str -> Result (Game, Str) _
undo_cmd = |game, _args|
    if List.len(game.board_history) >= 1 and game.force_mode == On then
        move = Result.with_default(List.last(game.move_history), 0)
        game_after_unmake = Game.unmake_move(game)
        Ok((game_after_unmake, Util.debug(game_after_unmake, "Undo move ${Move.to_str(move)}")))
    else
        Err(NotLegal)

expect
    # Given
    original = { initial_game & active_color: Black, force_mode: On, move_number: 1, move_history: [0], board_history: [initial_board] }
    # When
    actual = run_test(undo_cmd, original, [])
    # Then
    expected = { original & active_color: White, force_mode: On, move_number: 1, move_history: [], board_history: [] }
    actual == expected

# ----------------------------------------------------------------------------

usermove_cmd : Game, List Str -> Result (Game, Str) _
usermove_cmd = |game, args|
    when List.first(args) is
        Ok(str) ->
            when MoveParser.parse(game.board, game.active_color, str) is
                Ok(user_move) ->
                    # Make user move
                    game_after_user_move = Game.make_move(game, user_move)
                    if game_after_user_move.force_mode == Off then
                        # Make engine move
                        make_engine_move(game_after_user_move)
                    else
                        Ok((game_after_user_move, ""))

                Err(IllegalMove) -> Err(IllegalMove(str))
                Err(SyntaxError) -> Err(IllegalMove(str))

        Err(ListWasEmpty) -> Err(SyntaxError)

# Force move On
expect
    # Given
    original = { initial_game & force_mode: On }
    # When
    actual = run_test(usermove_cmd, original, ["e2e4"])
    # Then
    actual.force_mode == On and actual.active_color == Black and actual.move_number == 1 and List.len(actual.move_history) == 1

# Force move Off
expect
    # Given
    original = initial_game
    # When
    actual = run_test(usermove_cmd, original, ["e2e4"])
    # Then
    actual.force_mode == Off and actual.active_color == White and actual.move_number == 2 and List.len(actual.move_history) == 2

# ----------------------------------------------------------------------------

xboard_cmd : Game, List Str -> Result (Game, Str) _
xboard_cmd = |game, _args|
    Ok((game, ""))

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

## Find and make the engine move given the gameBeforeMove.
make_engine_move = |game_before_move|
    time_left = game_before_move.time_left
    moves_left = game_before_move.moves_left
    time_for_move = allocate_time_for_move(game_before_move.time_control, time_left, moves_left)
    time_text = format_time_text(game_before_move, time_left, moves_left, time_for_move)
    engine_move = Finder.find_move(game_before_move.board, game_before_move.board_history, game_before_move.active_color)
    when engine_move is
        FoundMove({ move: move, score: score }) ->
            game_after_move = update_time_data(Game.make_move(game_before_move, move))
            move_text = format_move(game_after_move, move, score)
            Ok((game_after_move, Str.concat(time_text, move_text)))

        Mated ->
            if
                game_before_move.active_color == White
            then
                Ok((game_before_move, "0-1 {Black mates}"))
            else
                Ok((game_before_move, "1-0 {White mates}"))

        Draw -> Ok((game_before_move, "1/2-1/2 {Stalemate}"))

## Returns the game with updated time data, that is, updated number of
## moves and time left to the time control. This function does not subtract
## used time from timeLeft, it only deals with time control updates.
update_time_data = |game|
    if game.time_control.type == Classic then
        if game.moves_left == 1 then
            # If the last move before the time control was made,
            # add extra time and reset number of moves left
            { game & moves_left: game.time_control.moves, time_left: game.time_left + game.time_control.base }
        else
            # Otherwise, just reduce number of moves left to time control
            { game & moves_left: game.moves_left - 1 }
    else
        # For incremental games, add the time increment
        { game & time_left: (game.time_left + game.time_control.inc) }

expect update_time_data(initial_game) == { initial_game & moves_left: initial_game.moves_left - 1 }
expect
    update_time_data({ initial_game & moves_left: 1, time_left: 1 })
    ==
    { initial_game & time_left: initial_game.time_control.base + 1 }

format_move : Game, Move, I64 -> Str
format_move = |game, move, score|
    Util.debug(game, "Found move ${Move.to_str(move)} with score ${Num.to_str(((Num.to_f64(score)) / 1000.0))}")
    |> Str.concat("move ${Move.to_str(move)}")

format_time_text = |game, time_left, moves_left, time_for_move|
    Util.debug(game, "Time left: ${Time.format_time(time_left)}, moves left: ${Num.to_str(moves_left)}, time for move: ${Time.format_time(time_for_move)}")

## Run 'fun' with the given game and args, and return the resulting game.
run_test = |fun, game, args|
    when fun(game, args) is
        Ok((g, _)) -> g
        _ -> initial_game
