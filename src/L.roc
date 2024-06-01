module [or]

or : List U64 -> U64
or = \list ->
    List.walk list 0 Num.bitwiseOr

expect or [1, 2, 4] == 7
