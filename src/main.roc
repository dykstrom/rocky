app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
}

import cli.Stderr
import cli.Stdin
import cli.Stdout
import cli.Utc

import Command
import Game exposing [initialGame]
import MoveParser
import Time exposing [formatTime]
import Util
import Version exposing [version]

# ----------------------------------------------------------------------------
# Xboard comands
# ----------------------------------------------------------------------------

commands = Dict.fromList [
    ("accepted", Command.acceptedCmd),
    ("board", Command.boardCmd),
    ("computer", Command.computerCmd),
    ("force", Command.forceCmd),
    ("go", Command.goCmd),
    ("help", Command.helpCmd),
    ("level", Command.levelCmd),
    ("new", Command.newCmd),
    ("otim", Command.otimCmd),
    ("ping", Command.pingCmd),
    ("plain", Command.plainCmd),
    ("playother", Command.playOtherCmd),
    ("protover", Command.protoverCmd),
    ("rejected", Command.rejectedCmd),
    ("remove", Command.removeCmd),
    ("result", Command.resultCmd),
    ("setboard", Command.setBoardCmd),
    ("time", Command.timeCmd),
    ("undo", Command.undoCmd),
    ("usermove", Command.usermoveCmd),
    ("xboard", Command.xboardCmd),
]

# ----------------------------------------------------------------------------
# Execution
# ----------------------------------------------------------------------------

execute = \game, cmd, args ->
    when Dict.get commands cmd is
        Ok fun -> fun game args
        _ -> if MoveParser.isMove cmd then Command.usermoveCmd game [cmd] else Err (UnknownCommand cmd)

executeAndFormat = \game, cmd, args, text ->
    when execute game cmd args is
        Ok result -> result
        Err (IllegalMove msg) -> (game, "Illegal move: $(msg)")
        Err IllegalPosition -> (game, "tellusererror Illegal position")
        Err SyntaxError -> (game, "Error (syntax error): $(text)")
        Err NotLegal -> (game, "Error (command not legal now): $(text)")
        Err (UnknownCommand msg) -> (game, "Error (unknown command): $(msg)")

# ----------------------------------------------------------------------------
# Main program
# ----------------------------------------------------------------------------

loop = \game ->
    input = Stdin.line!
    parts = Str.splitOn input " "
    args = List.sublist parts { start: 1, len: Num.maxU64 }
    when List.first parts is
        Ok cmd if cmd == "quit" ->
            Task.ok (Done {})

        Ok cmd ->
            startTime = Utc.now! {}
            tuple = executeAndFormat game cmd args input
            endTime = Utc.now! {}
            runTime = Num.toI128 (Utc.deltaAsMillis startTime endTime)
            gameAfterMove = tuple.0
            msgAfterMove = tuple.1
            timeLeft = tuple.0.timeLeft - runTime
            finalGame = { gameAfterMove & timeLeft: timeLeft }
            Stdout.line! msgAfterMove
            Stdout.line! (Util.debug finalGame "Executed '$(input)' in $(formatTime runTime), time left: $(formatTime timeLeft)")
            Task.ok (Step finalGame)

        Err ListWasEmpty ->
            Task.err ListWasEmpty

run =
    Stdout.line! "# Welcome to rocky $(Str.trim version)"
    Stdout.line! "# Type 'help' to get help"
    Task.loop! initialGame loop
    Stdout.line! "# Bye"

main =
    run |> Task.onErr handleErr

handleErr = \error ->
    when error is
        StdinErr EndOfFile -> Stdout.line ""
        _ -> Stderr.line "Error: $(Inspect.toStr error)"
