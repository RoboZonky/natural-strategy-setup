module Data.Portfolio exposing (..)


type DefaultPortfolio
    = Conservative
    | Balanced
    | Progressive
    | Empty


toString : DefaultPortfolio -> String
toString pt =
    case pt of
        Conservative ->
            "konzervativní"

        Balanced ->
            "balancované"

        Progressive ->
            "progresivní"

        Empty ->
            "prázdné"


fromString : String -> DefaultPortfolio
fromString str =
    case str of
        "Conservative" ->
            Conservative

        "Balanced" ->
            Balanced

        "Progressive" ->
            Progressive

        _ ->
            Empty
