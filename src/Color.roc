module [Color, flip_color]

Color : [White, Black]

flip_color : Color -> Color
flip_color = |color|
    if color == White then Black else White

expect flip_color(White) == Black
expect flip_color(Black) == White
