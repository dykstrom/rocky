module [or_list]

or_list : List U64 -> U64
or_list = |list|
    List.walk list 0 Num.bitwise_or

expect or_list [1, 2, 4] == 7
