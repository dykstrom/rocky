app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.11.0/SY4WWMhWQ9NvQgvIthcv15AUeA7rAIJHAHgiaSHGhdY.tar.br",
}

import Board exposing [initialBoard]
import Color
import Finder
import Game exposing [Game, initialGame]
import Move exposing [Move]
import MoveParser
import cli.Stderr
import cli.Stdin
import cli.Stdout
import cli.Task exposing [Task]

import "../data/version.txt" as version : Str

# ----------------------------------------------------------------------------
# Xboard comands
# ----------------------------------------------------------------------------

acceptedCmd = \game, args ->
    when args is
        ["debug"] -> Ok ({ game & debug: On }, "")
        _ -> Ok (game, "")

expect acceptedCmd { debug: Off } ["debug"] == Ok ({ debug: On }, "")
expect acceptedCmd { debug: Off } ["ping"] == Ok ({ debug: Off }, "")

boardCmd = \game, _args ->
    Ok (game, Board.toStr game.board)

computerCmd = \game, _args ->
    Ok (game, "")

forceCmd = \game, _args ->
    Ok ({ game & forceMode: On }, "")

expect forceCmd { forceMode: Off } [] == Ok ({ forceMode: On }, "")

goCmd = \game, _args ->
    gameBeforeMove = { game & forceMode: Off, engineColor: game.activeColor }
    # Make engine move
    makeEngineMove gameBeforeMove

expect
    # Given
    original = { initialGame & forceMode: On, activeColor: White }
    # When
    actual = runTest goCmd original []
    # Then
    actual.forceMode
    == Off
    && actual.activeColor
    == Black
    && actual.engineColor
    == White
    && actual.moveNumber
    == 1
    && List.len actual.moveHistory
    == 1

helpText =
    """
    Available commands
    ------------------
    board     = show current position
    force     = turn force mode on
    go        = turn force mode off and set the chess engine to
                play the color that is on move
    help      = show this help text
    new       = start a new game with the chess engine as black
    ping      = ping the chess engine
    playother = turn force mode off and set the chess engine to
                play the color that is not on move
    quit      = quit program
    remove    = retract latest move pair, and let the user move again
    usermove  = submit a move in coordinate algebraic notation
    xboard    = put the engine in xboard mode
    """

helpCmd = \game, _args ->
    Ok (game, helpText)

newCmd = \game, _args ->
    # Keep feature settings
    Ok ({ initialGame & debug: game.debug }, "")

expect newCmd { debug: On } [] == Ok ({ initialGame & debug: On }, "")

otimCmd = \game, _args ->
    Ok (game, "")

pingCmd = \game, args ->
    when args is
        [arg] -> Ok (game, "pong $(arg)")
        _ -> Err SyntaxError

expect pingCmd {} ["1"] == Ok ({}, "pong 1")
expect pingCmd {} [] == Err SyntaxError

playOtherCmd = \game, _args ->
    Ok ({ game & forceMode: Off, engineColor: Color.flipColor game.activeColor }, "")

expect
    playOtherCmd { forceMode: On, activeColor: Black, engineColor: Black } []
    ==
    Ok ({ forceMode: Off, activeColor: Black, engineColor: White }, "")

protoverCmd = \game, _args ->
    Ok (game, "feature ping=1\nfeature setboard=0\nfeature playother=1\nfeature san=0\nfeature usermove=1\nfeature time=1\nfeature draw=0\nfeature sigint=0\nfeature sigterm=0\nfeature reuse=1\nfeature analyze=0\nfeature myname=\"rocky $(Str.trim version)\"\nfeature variants=\"normal\"\nfeature colors=0\nfeature ics=0\nfeature name=0\nfeature pause=0\nfeature debug=1\nfeature done=1")

rejectedCmd = \game, _args ->
    Ok (game, "")

removeCmd = \game, _args ->
    if List.len game.boardHistory >= 2 then
        gameAfterUnmake = Game.unmakeMove game |> Game.unmakeMove
        Ok (gameAfterUnmake, "")
    else
        Err NotLegal

expect
    # Given
    original = { initialGame & moveNumber: 2, moveHistory: [0, 0], boardHistory: [initialBoard, initialBoard] }
    # When
    actual = runTest removeCmd original []
    # Then
    expected = { original & moveNumber: 1, moveHistory: [], boardHistory: [] }
    actual == expected

resultCmd = \game, _args ->
    Ok (game, "")

timeCmd = \game, _args ->
    Ok (game, "")

usermoveCmd = \game, args ->
    when List.first args is
        Ok str ->
            when MoveParser.parse game.board game.activeColor str is
                Ok userMove ->
                    # Make user move
                    gameAfterUserMove = Game.makeMove game userMove
                    if gameAfterUserMove.forceMode == Off then
                        # Make engine move
                        makeEngineMove gameAfterUserMove
                    else
                        Ok (gameAfterUserMove, "")

                Err IllegalMove -> Err (IllegalMove str)
                Err SyntaxError -> Err (IllegalMove str)

        Err ListWasEmpty -> Err SyntaxError

## Find and make the engine move given the gameBeforeMove.
makeEngineMove = \gameBeforeMove ->
    engineMove = Finder.findMove gameBeforeMove.board gameBeforeMove.boardHistory gameBeforeMove.activeColor
    when engineMove is
        FoundMove { move: move, score: score } ->
            gameAfterMove = Game.makeMove gameBeforeMove move
            Ok (gameAfterMove, formatMove gameAfterMove move score)

        Mated ->
            if
                gameBeforeMove.activeColor == White
            then
                Ok (gameBeforeMove, "0-1 {Black mates}")
            else
                Ok (gameBeforeMove, "1-0 {White mates}")

        Draw -> Ok (gameBeforeMove, "1/2-1/2 {Stalemate}")

formatMove : Game, Move, I64 -> Str
formatMove = \game, move, score ->
    debug game "Found move $(Move.toStr move) with score $(Num.toStr ((Num.toF64 score) / 1000.0))"
    |> Str.concat "move $(Move.toStr move)"

# Force move On
expect
    # Given
    original = { initialGame & forceMode: On }
    # When
    actual = runTest usermoveCmd original ["e2e4"]
    # Then
    actual.forceMode
    == On
    && actual.activeColor
    == Black
    && actual.moveNumber
    == 1
    && List.len actual.moveHistory
    == 1

# Force move Off
expect
    # Given
    original = initialGame
    # When
    actual = runTest usermoveCmd original ["e2e4"]
    # Then
    actual.forceMode
    == Off
    && actual.activeColor
    == White
    && actual.moveNumber
    == 2
    && List.len actual.moveHistory
    == 2

xboardCmd = \game, _args ->
    Ok (game, "")

commands = Dict.fromList [
    ("accepted", acceptedCmd),
    ("board", boardCmd),
    ("computer", computerCmd),
    ("force", forceCmd),
    ("go", goCmd),
    ("help", helpCmd),
    ("new", newCmd),
    ("otim", otimCmd),
    ("ping", pingCmd),
    ("playother", playOtherCmd),
    ("protover", protoverCmd),
    ("rejected", rejectedCmd),
    ("remove", removeCmd),
    ("result", resultCmd),
    ("time", timeCmd),
    ("usermove", usermoveCmd),
    ("xboard", xboardCmd),
]

# ----------------------------------------------------------------------------
# Execution
# ----------------------------------------------------------------------------

execute = \game, cmd, args ->
    when Dict.get commands cmd is
        Ok fun -> fun game args
        _ -> Err (UnknownCommand cmd)

executeAndFormat = \game, cmd, args, text ->
    when execute game cmd args is
        Ok result -> (Ok (Step result.0), result.1)
        Err (IllegalMove msg) -> (Ok (Step game), "Illegal move: $(msg)")
        Err SyntaxError -> (Ok (Step game), "Error (syntax error): $(text)")
        Err NotLegal -> (Ok (Step game), "Error (command not legal now): $(text)")
        Err (UnknownCommand msg) -> (Ok (Step game), "Error (unknown command): $(msg)")

# ----------------------------------------------------------------------------
# Main program
# ----------------------------------------------------------------------------

loop = \game ->
    input = Stdin.line!
    parts = Str.split input " "
    args = List.sublist parts { start: 1, len: Num.maxU64 }
    when List.first parts is
        Ok cmd if cmd == "quit" ->
            Task.fromResult (Ok (Done game))

        Ok cmd ->
            tuple = executeAndFormat game cmd args input
            {} <- Stdout.line tuple.1 |> Task.await
            Task.fromResult tuple.0

        Err ListWasEmpty ->
            Task.fromResult (Err ListWasEmpty)

run =
    Stdout.line! "# Welcome to rocky $(version)"
    Stdout.line! "# Type 'help' to get help"
    Stdout.line! "# Type 'quit' to quit"
    Task.loop! initialGame loop
    Stdout.line! "# Bye"

main =
    run |> Task.onErr handleErr

handleErr = \error ->
    when error is
        StdinErr EndOfFile -> Stdout.line ""
        _ -> Stderr.line "Error: $(Inspect.toStr error)"

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

debug : Game, Str -> Str
debug = \game, msg ->
    if game.debug == On then
        Str.concat "# " msg |> Str.concat "\n"
    else
        ""

## Run 'fun' with the given game and args, and return the resulting game.
runTest = \fun, game, args ->
    (Result.withDefault (fun game args) (initialGame, "")).0
