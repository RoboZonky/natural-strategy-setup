module Data.InvestmentShare exposing
    ( InvestmentShare(..)
    , decoder
    , encode
    , render
    , validate
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type InvestmentShare
    = NotSpecified --in practice bounded by Zonky, which permits max 5000 Kč
    | Percent Int


render : InvestmentShare -> String
render investmentShare =
    case investmentShare of
        Percent share ->
            "Investovat maximálně " ++ String.fromInt share ++ " % výše úvěru."

        NotSpecified ->
            ""


validate : InvestmentShare -> List String
validate s =
    case s of
        Percent pct ->
            Util.validate (pct < 1 || 100 < pct) "Podíl výše úvěru musí být mezi 1 a 100 %"

        NotSpecified ->
            []



-- JSON


encode : InvestmentShare -> Value
encode is =
    case is of
        Percent pct ->
            Encode.list Encode.int [ 1, pct ]

        NotSpecified ->
            Encode.list Encode.int [ 2 ]


decoder : Decoder InvestmentShare
decoder =
    Decode.list Decode.int
        |> Decode.andThen
            (\ints ->
                case ints of
                    [ 1, pct ] ->
                        Decode.succeed <| Percent pct

                    [ 2 ] ->
                        Decode.succeed NotSpecified

                    _ ->
                        Decode.fail <| "Unable to decode InvestmentShare from " ++ Util.intListToString ints
            )
