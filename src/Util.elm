module Util exposing (..)


orList : (a -> String) -> List a -> String
orList itemToString list =
    case list of
        [] ->
            "VYBERTE ASPOŇ JEDNU HODNOTU"

        r :: [] ->
            itemToString r

        r1 :: r2 :: [] ->
            itemToString r1 ++ " nebo " ++ itemToString r2

        r1 :: rest ->
            itemToString r1 ++ ", " ++ orList itemToString rest


joinNonemptyLines : List String -> String
joinNonemptyLines =
    String.join "\n" << List.filter (not << String.isEmpty)
