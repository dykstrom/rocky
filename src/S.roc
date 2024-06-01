module [indexOf, len, substr, toUpper]

## Return a substring of the given string.
## Note: This function only works for ASCII strings.
substr : Str, { start : U64, len : U64 } -> Result Str [BadUtf8]
substr = \str, spec ->
    str
    |> Str.toUtf8
    |> List.sublist spec
    |> Str.fromUtf8
    |> Result.mapErr \_ -> BadUtf8

expect substr "e2e4" { start: 0, len: 2 } == Ok "e2"
expect substr "e2e4" { start: 2, len: 2 } == Ok "e4"
expect substr "e7e8q" { start: 4, len: 1 } == Ok "q"
expect substr "e2e4" { start: 4, len: 1 } == Ok ""

## Return the index of the first occurrence of the second string in the first string.
## If the second string is not found in the first string, return 'Err NotFound'.
## Note: This function only works for ASCII strings.
indexOf : Str, Str -> Result U64 [BadUtf8, NotFound]
indexOf = \string, substring ->
    iter = \list, index ->
        if List.isEmpty list then
            Err NotFound
        else
            when Str.fromUtf8 list is
                Ok s if Str.startsWith s substring -> Ok index
                Ok _ -> List.sublist list { start: 1, len: Num.maxU64 } |> iter (index + 1)
                Err _ -> Err BadUtf8
    Str.toUtf8 string |> iter 0

expect indexOf "foo" "bar" == Err NotFound
expect indexOf "foo" "f" == Ok 0
expect indexOf "foo" "oo" == Ok 1
expect indexOf "foobar" "ooo" == Err NotFound
expect indexOf "foobar" "foobar" == Ok 0
expect indexOf "foobar" "r" == Ok 5
expect indexOf "foobarf" "f" == Ok 0
expect indexOf "foobarf" "" == Ok 0

## Return the string converted to upper case.
## Note: This function only works for ASCII strings.
toUpper : Str -> Result Str [BadUtf8]
toUpper = \str ->
    str
    |> Str.toUtf8
    |> List.map \c -> if c >= 97 && c <= 122 then c - 32 else c
    |> Str.fromUtf8
    |> Result.mapErr \_ -> BadUtf8

expect toUpper "abc" == Ok "ABC"
expect toUpper "a0z9" == Ok "A0Z9"

## Return length of string.
## Note: This function only works for ASCII strings.
len : Str -> U64
len = \str ->
    Str.countUtf8Bytes str

expect len "" == 0
expect len "abc" == 3
