module [TimeControl, initial_time_control, allocate_time_for_move, format_time, parse_time_control]

TimeControl : {
    type : [Classic, Incremental],
    moves : I128,
    base : I128,
    inc : I128,
}

initial_time_control : TimeControl
initial_time_control = {
    type: Classic,
    moves: 40,
    base: 120_000,
    inc: 0,
}

parse_time_control : Str, Str, Str -> Result TimeControl [SyntaxError]
parse_time_control = |mps, base, inc|
    when [parse_mps(mps), parse_base(base), parse_inc(inc)] is
        [Ok(moves), Ok(base_time), Ok(increment)] ->
            if moves != 0 then
                Ok({ type: Classic, moves: moves, base: base_time, inc: 0 })
            else
                Ok({ type: Incremental, moves: 0, base: base_time, inc: increment })

        _ ->
            Err(SyntaxError)

expect parse_time_control("20", "5", "0") == Ok({ type: Classic, moves: 20, base: 300_000, inc: 0 })
expect parse_time_control("17", "0:15", "0") == Ok({ type: Classic, moves: 17, base: 15_000, inc: 0 })
expect parse_time_control("0", "2", "10") == Ok({ type: Incremental, moves: 0, base: 120_000, inc: 10_000 })

## Return number of moves per time control.
parse_mps = |mps|
    Str.to_i128(mps)

expect parse_mps("20") == Ok(20)

## Return base time in millis.
parse_base = |base|
    parts = Str.split_on(base, ":")
    if List.is_empty(parts) then
        Err(SyntaxError)
    else
        m1 =
            List.get(parts, 0)
            |> Result.try(Str.to_i128)
            |> Result.map_ok(|minutes| minutes * 60_000)
            |> Result.with_default(0)
        s1 =
            if List.len(parts) > 1 then
                List.get(parts, 1)
                |> Result.try(Str.to_i128)
                |> Result.map_ok(|seconds| seconds * 1_000)
                |> Result.with_default(0)
            else
                0i128
        Ok(m1 + s1)

expect parse_base("10") == Ok(600_000)
expect parse_base("1:30") == Ok(90_000)

## Return time increment in millis.
parse_inc = |inc|
    Str.to_i128(inc)
    |> Result.map_ok(|seconds| seconds * 1_000)

expect parse_inc("3") == Ok(3_000)

## Returns the time to use for this move.
allocate_time_for_move : TimeControl, I128, I128 -> I128
allocate_time_for_move = |time_control, time_left, moves_left|
    if time_control.type == Classic then
        if moves_left == 0 then
            crash("Should not happen: movesLeft = 0! Time left: ${format_time(time_left)}, time control: ${Inspect.to_str(time_control)}")
        else if moves_left == 1 then
            # If this is the last move before the time control,
            # be conservative about the time to use
            time_left // 2
        else
            time_left // moves_left
    else
        # For incremental time control, assume there are 40 moves left
        time_left // 40 + time_control.inc

expect allocate_time_for_move(initial_time_control, 80_000, 20) == 4_000
expect allocate_time_for_move(initial_time_control, 80_000, 1) == 40_000
expect allocate_time_for_move({ type: Incremental, base: 40_000, inc: 1_000, moves: 0 }, 8_000, 0) == 1_200

format_time : I128 -> Str
format_time = |time_in_ms|
    "${Num.to_str((Num.to_f64(time_in_ms) / 1_000))}s"
