module [index_of, len, substr, to_upper]

## Return a substring of the given string.
## Note: This function only works for ASCII strings.
substr : Str, { start : U64, len : U64 } -> Result Str [BadUtf8]
substr = |str, spec|
    str
    |> Str.to_utf8
    |> List.sublist(spec)
    |> Str.from_utf8
    |> Result.map_err(|_| BadUtf8)

expect substr("e2e4", { start: 0, len: 2 }) == Ok("e2")
expect substr("e2e4", { start: 2, len: 2 }) == Ok("e4")
expect substr("e7e8q", { start: 4, len: 1 }) == Ok("q")
expect substr("e2e4", { start: 4, len: 1 }) == Ok("")

## Return the index of the first occurrence of the second string in the first string.
## If the second string is not found in the first string, return 'Err NotFound'.
## Note: This function only works for ASCII strings.
index_of : Str, Str -> Result U64 [BadUtf8, NotFound]
index_of = |string, substring|
    iter = |list, index|
        if List.is_empty(list) then
            Err(NotFound)
        else
            when Str.from_utf8(list) is
                Ok(s) if Str.starts_with(s, substring) -> Ok(index)
                Ok(_) -> List.sublist(list, { start: 1, len: Num.max_u64 }) |> iter((index + 1))
                Err(_) -> Err(BadUtf8)
    Str.to_utf8(string) |> iter(0)

expect index_of("foo", "bar") == Err(NotFound)
expect index_of("foo", "f") == Ok(0)
expect index_of("foo", "oo") == Ok(1)
expect index_of("foobar", "ooo") == Err(NotFound)
expect index_of("foobar", "foobar") == Ok(0)
expect index_of("foobar", "r") == Ok(5)
expect index_of("foobarf", "f") == Ok(0)
expect index_of("foobarf", "") == Ok(0)

## Return the string converted to upper case.
## Note: This function only works for ASCII strings.
to_upper : Str -> Result Str [BadUtf8]
to_upper = |str|
    str
    |> Str.to_utf8
    |> List.map(|c| if c >= 97 and c <= 122 then c - 32 else c)
    |> Str.from_utf8
    |> Result.map_err(|_| BadUtf8)

expect to_upper("abc") == Ok("ABC")
expect to_upper("a0z9") == Ok("A0Z9")

## Return length of string.
## Note: This function only works for ASCII strings.
len : Str -> U64
len = |str|
    Str.count_utf8_bytes(str)

expect len("") == 0
expect len("abc") == 3
