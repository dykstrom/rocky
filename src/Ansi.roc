module [dark, default, light]

# ANSI colors
blackOnGreen = Str.fromUtf8 [27] |> Result.map (\esc -> Str.concat esc "[30;102m") |> Result.withDefault "?"
blackOnYellow = Str.fromUtf8 [27] |> Result.map (\esc -> Str.concat esc "[30;103m") |> Result.withDefault "?"
default = Str.fromUtf8 [27] |> Result.map (\esc -> Str.concat esc "[0m") |> Result.withDefault "?"

dark = blackOnGreen
light = blackOnYellow
