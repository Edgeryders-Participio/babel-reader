module ParserUtil exposing (deadEndsToString)

import Parser exposing (..)


deadEndsToString : List DeadEnd -> String -> String
deadEndsToString deadEnds source =
    let
        lines =
            String.lines source
    in
    String.concat (List.intersperse "; " (List.map (deadEndToString lines) deadEnds))


deadEndToString : List String -> DeadEnd -> String
deadEndToString lines deadend =
    problemToString deadend.problem
        ++ " at row "
        ++ String.fromInt deadend.row
        ++ ", col "
        ++ String.fromInt deadend.col
        ++ " while parsing \""
        ++ sourceSnippet deadend.row deadend.col lines
        ++ "\""


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting hex"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"


sourceSnippet : Int -> Int -> List String -> String
sourceSnippet row col lines =
    let
        line =
            List.drop (row - 1) lines |> List.take 1
    in
    case line of
        [ srcRow ] ->
            String.dropLeft (col - 1) srcRow
                |> String.left 50

        _ ->
            "(bad location)"
