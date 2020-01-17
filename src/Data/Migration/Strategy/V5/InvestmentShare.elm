module Data.Migration.Strategy.V5.InvestmentShare exposing
    ( InvestmentShare(..)
    , decoder
    , render
    )

import Json.Decode as Decode exposing (Decoder)
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
