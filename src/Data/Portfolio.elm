module Data.Portfolio
    exposing
        ( Portfolio(..)
        , renderPortfolio
        , fromString
        , toString
        )


type Portfolio
    = Conservative
    | Balanced
    | Progressive
    | Empty


toString : Portfolio -> String
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


fromString : String -> Portfolio
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


renderPortfolio : Portfolio -> String
renderPortfolio portfolio =
    "Robot má udržovat " ++ toString portfolio ++ " portfolio."
