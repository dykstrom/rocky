module [TimeControl, initialTimeControl, allocateTimeForMove, formatTime, parseTimeControl]

TimeControl : {
    type : [Classic, Incremental],
    moves : I128,
    base : I128,
    inc : I128,
}

initialTimeControl : TimeControl
initialTimeControl = {
    type: Classic,
    moves: 40,
    base: 120_000,
    inc: 0,
}

parseTimeControl : Str, Str, Str -> Result TimeControl [SyntaxError]
parseTimeControl = \mps, base, inc ->
    when [parseMps mps, parseBase base, parseInc inc] is
        [Ok moves, Ok baseTime, Ok increment] ->
            if moves != 0 then
                Ok { type: Classic, moves: moves, base: baseTime, inc: 0 }
            else
                Ok { type: Incremental, moves: 0, base: baseTime, inc: increment }

        _ ->
            Err SyntaxError

expect parseTimeControl "20" "5" "0" == Ok { type: Classic, moves: 20, base: 300_000, inc: 0 }
expect parseTimeControl "17" "0:15" "0" == Ok { type: Classic, moves: 17, base: 15_000, inc: 0 }
expect parseTimeControl "0" "2" "10" == Ok { type: Incremental, moves: 0, base: 120_000, inc: 10_000 }

## Return number of moves per time control.
parseMps = \mps ->
    Str.toI128 mps

expect parseMps "20" == Ok 20

## Return base time in millis.
parseBase = \base ->
    parts = Str.splitOn base ":"
    if List.isEmpty parts then
        Err SyntaxError
    else
        m1 =
            List.get parts 0
            |> Result.try \m -> Str.toI128 m
            |> Result.map \minutes -> minutes * 60_000
            |> Result.withDefault 0
        s1 =
            if List.len parts > 1 then
                List.get parts 1
                |> Result.try \s -> Str.toI128 s
                |> Result.map \seconds -> seconds * 1_000
                |> Result.withDefault 0
            else
                0i128
        Ok (m1 + s1)

expect parseBase "10" == Ok 600_000
expect parseBase "1:30" == Ok 90_000

## Return time increment in millis.
parseInc = \inc ->
    Str.toI128 inc
    |> Result.map \seconds -> seconds * 1_000

expect parseInc "3" == Ok 3_000

## Returns the time to use for this move.
allocateTimeForMove : TimeControl, I128, I128 -> I128
allocateTimeForMove = \timeControl, timeLeft, movesLeft ->
    if timeControl.type == Classic then
        if movesLeft == 0 then
            crash "Should not happen: movesLeft = 0! Time left: $(formatTime timeLeft), time control: $(Inspect.toStr timeControl)"
        else if movesLeft == 1 then
            # If this is the last move before the time control,
            # be conservative about the time to use
            timeLeft // 2
        else
            timeLeft // movesLeft
    else
        # For incremental time control, assume there are 40 moves left
        timeLeft // 40 + timeControl.inc

expect allocateTimeForMove initialTimeControl 80_000 20 == 4_000
expect allocateTimeForMove initialTimeControl 80_000 1 == 40_000
expect allocateTimeForMove { type: Incremental, base: 40_000, inc: 1_000, moves: 0 } 8_000 0 == 1_200

formatTime : I128 -> Str
formatTime = \timeInMs ->
    "$(Num.toStr (Num.toF64 timeInMs / 1_000))s"
