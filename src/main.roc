app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
}

import cli.Stderr
import cli.Stdin
import cli.Stdout
import cli.Utc

import Command
import Game exposing [initial_game]
import MoveParser
import Time exposing [format_time]
import Util
import Version exposing [version]

# ----------------------------------------------------------------------------
# Xboard comands
# ----------------------------------------------------------------------------

commands = Dict.from_list(
    [
        ("accepted", Command.accepted_cmd),
        ("board", Command.board_cmd),
        ("computer", Command.computer_cmd),
        ("force", Command.force_cmd),
        ("go", Command.go_cmd),
        ("help", Command.help_cmd),
        ("level", Command.level_cmd),
        ("new", Command.new_cmd),
        ("otim", Command.otim_cmd),
        ("ping", Command.ping_cmd),
        ("plain", Command.plain_cmd),
        ("playother", Command.play_other_cmd),
        ("protover", Command.protover_cmd),
        ("rejected", Command.rejected_cmd),
        ("remove", Command.remove_cmd),
        ("result", Command.result_cmd),
        ("setboard", Command.set_board_cmd),
        ("time", Command.time_cmd),
        ("undo", Command.undo_cmd),
        ("usermove", Command.usermove_cmd),
        ("xboard", Command.xboard_cmd),
    ],
)

# ----------------------------------------------------------------------------
# Execution
# ----------------------------------------------------------------------------

execute = |game, cmd, args|
    when Dict.get(commands, cmd) is
        Ok(fun) -> fun(game, args)
        _ -> if MoveParser.is_move(cmd) then Command.usermove_cmd(game, [cmd]) else Err(UnknownCommand(cmd))

execute_and_format = |game, cmd, args, text|
    when execute(game, cmd, args) is
        Ok((game_after_cmd, msg)) -> (game_after_cmd, msg)
        Err(IllegalMove(msg)) -> (game, "Illegal move: ${msg}")
        Err(IllegalPosition) -> (game, "tellusererror Illegal position")
        Err(SyntaxError) -> (game, "Error (syntax error): ${text}")
        Err(NotLegal) -> (game, "Error (command not legal now): ${text}")
        Err(UnknownCommand(msg)) -> (game, "Error (unknown command): ${msg}")

# ----------------------------------------------------------------------------
# Main program
# ----------------------------------------------------------------------------

loop! = |game|
    input = Stdin.line!? {}
    parts = Str.split_on(input, " ")
    args = List.sublist(parts, { start: 1, len: Num.max_u64 })
    when List.first(parts) is
        Ok(cmd) if cmd == "quit" ->
            Ok({})

        Ok(cmd) ->
            start_time = Utc.now!({})
            tuple = execute_and_format(game, cmd, args, input)
            end_time = Utc.now!({})
            run_time = Num.to_i128(Utc.delta_as_millis(start_time, end_time))
            game_after_move = tuple.0
            msg_after_move = tuple.1
            time_left = tuple.0.time_left - run_time
            final_game = { game_after_move & time_left: time_left }
            print!(msg_after_move)?
            print!(Util.debug(final_game, "Executed '${input}' in ${format_time(run_time)}, time left: ${format_time(time_left)}"))?
            loop!(final_game)

        Err(ListWasEmpty) ->
            Err(ListWasEmpty)

print! = |s|
    if Str.is_empty(s) then
        Ok({})
    else
        Stdout.line!(Str.trim(s))

main! = |_|
    Stdout.line!("# Welcome to rocky ${Str.trim(version)}")?
    Stdout.line!("# Type 'help' to get help")?
    result = loop!(initial_game)
    when result is
        Ok(_) | Err(EndOfFile) -> Stdout.line!("# Bye")
        _ -> Stderr.line!("Error: ${Inspect.to_str(result)}")
