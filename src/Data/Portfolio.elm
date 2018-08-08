module Data.Portfolio
    exposing
        ( Portfolio(..)
        , decoder
        , encode
        , fromString
        , render
        , toUiLabel
        )

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Util


type Portfolio
    = Conservative
    | Balanced
    | Progressive
    | Empty


allPortfolios : List Portfolio
allPortfolios =
    [ Conservative, Balanced, Progressive, Empty ]


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


{-| Unlike toString (used to render strategy config) this uses more
"user friendly" name for Empty portfolio.
-}
toUiLabel : Portfolio -> String
toUiLabel pt =
    case pt of
        Conservative ->
            "konzervativní"

        Balanced ->
            "balancované"

        Progressive ->
            "progresivní"

        Empty ->
            "mnou definované"


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


render : Portfolio -> String
render portfolio =
    "Robot má udržovat " ++ toString portfolio ++ " portfolio."



-- JSON


encode : Portfolio -> Value
encode =
    Util.enumEncoder allPortfolios


decoder : Decoder Portfolio
decoder =
    Util.enumDecoder "Portfolio" allPortfolios
