module [Color, flipColor]

Color : [White, Black]

flipColor : Color -> Color
flipColor = \color ->
    if color == White then Black else White

expect flipColor White == Black
expect flipColor Black == White
