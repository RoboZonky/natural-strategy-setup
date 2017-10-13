module Data.Portfolio
    exposing
        ( Portfolio(..)
        , decoder
        , encode
        , fromString
        , renderPortfolio
        , toString
        )

import Json.Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


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



-- JSON


encode : Portfolio -> Value
encode =
    Encode.string << Basics.toString


decoder : Decoder Portfolio
decoder =
    Util.enumDecoder [ Conservative, Balanced, Progressive, Empty ]
