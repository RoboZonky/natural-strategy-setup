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
    | UserDefined


allPortfolios : List Portfolio
allPortfolios =
    [ Conservative
    , Balanced
    , Progressive
    , UserDefined
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

        UserDefined ->
            "uživatelem definované"


{-| Unlike toStrategyString (used for strategy config rendering) this uses more
user friendly name for UserDefined portfolio.
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

        UserDefined ->
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

        UserDefined ->
            "UserDefined"


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
            UserDefined


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
