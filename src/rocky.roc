app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.11.0/SY4WWMhWQ9NvQgvIthcv15AUeA7rAIJHAHgiaSHGhdY.tar.br",
}

import Board exposing [initialBoard]
import Color
import Finder
import Game exposing [initialGame]
import Move
import MoveParser
import cli.Stderr
import cli.Stdin
import cli.Stdout
import cli.Task exposing [Task]

import "../data/version.txt" as version : Str

# ----------------------------------------------------------------------------
# Xboard comands
# ----------------------------------------------------------------------------

acceptedCmd = \game, _args ->
    Ok (game, "")

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

newCmd = \_game, _args ->
    Ok (initialGame, "")

expect newCmd {} [] == Ok (initialGame, "")

otimCmd = \game, _args ->
    Ok (game, "")

pingCmd = \game, args ->
    List.get args 0
    |> Result.map \arg -> (game, "pong $(arg)")
    |> Result.onErr \_ -> Err SyntaxError

expect pingCmd {} ["1"] == Ok ({}, "pong 1")

playOtherCmd = \game, _args ->
    Ok ({ game & forceMode: Off, engineColor: Color.flipColor game.activeColor }, "")

expect
    playOtherCmd { forceMode: On, activeColor: Black, engineColor: Black } []
    ==
    Ok ({ forceMode: Off, activeColor: Black, engineColor: White }, "")

protoverCmd = \game, _args ->
    Ok (game, "feature ping=1\nfeature setboard=0\nfeature playother=1\nfeature san=0\nfeature usermove=1\nfeature time=1\nfeature draw=0\nfeature sigint=0\nfeature sigterm=0\nfeature reuse=1\nfeature analyze=0\nfeature myname=\"rocky $(version)\"\nfeature variants=\"normal\"\nfeature colors=0\nfeature ics=0\nfeature name=0\nfeature pause=0\nfeature done=1")

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
        FoundMove { move: move, score: _ } ->
            gameAfterMove = Game.makeMove gameBeforeMove move
            Ok (gameAfterMove, "move $(Move.toStr move)")

        Mated ->
            if
                gameBeforeMove.activeColor == White
            then
                Ok (gameBeforeMove, "0-1 {Black mates}")
            else
                Ok (gameBeforeMove, "1-0 {White mates}")

        Draw -> Ok (gameBeforeMove, "1/2-1/2 {Stalemate}")

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
    ("new", newCmd),
    ("otim", otimCmd),
    ("ping", pingCmd),
    ("playOther", playOtherCmd),
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
    Stdout.line! "# Type 'quit' to quit."
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

## Run 'fun' with the given game and args, and return the resulting game.
runTest = \fun, game, args ->
    (Result.withDefault (fun game args) (initialGame, "")).0
