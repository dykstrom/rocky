module [dark, default, light]

# ANSI colors
black_on_green = Str.from_utf8([27]) |> Result.map_ok(|esc| Str.concat(esc, "[30;102m")) |> Result.with_default("?")
black_on_yellow = Str.from_utf8([27]) |> Result.map_ok(|esc| Str.concat(esc, "[30;103m")) |> Result.with_default("?")
default = Str.from_utf8([27]) |> Result.map_ok(|esc| Str.concat(esc, "[0m")) |> Result.with_default("?")

dark = black_on_green
light = black_on_yellow
