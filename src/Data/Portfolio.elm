module Data.Portfolio exposing
    ( Portfolio(..)
    , allPortfolios
    , decoder
    , encode
    , fromString
    , render
    , toString
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
    [ Conservative
    , Balanced
    , Progressive
    , Empty
    ]


toStrategyString : Portfolio -> String
toStrategyString pt =
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


{-| Used as value in portfolio dropdown.
Must be inverse of fromString.
-}
toString : Portfolio -> String
toString portfolio =
    case portfolio of
        Conservative ->
            "Conservative"

        Balanced ->
            "Balanced"

        Progressive ->
            "Progressive"

        Empty ->
            "Empty"


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
    "Robot má udržovat " ++ toStrategyString portfolio ++ " portfolio."



-- JSON


encode : Portfolio -> Value
encode =
    Util.enumEncoder allPortfolios


decoder : Decoder Portfolio
decoder =
    Util.enumDecoder "Portfolio" allPortfolios
