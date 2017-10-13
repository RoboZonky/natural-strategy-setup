module Data.InvestmentShare
    exposing
        ( InvestmentShare(..)
        , decoder
        , encode
        , renderInvestmentShare
        , validate
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type InvestmentShare
    = NotSpecified --in practice bounded by Zonky, which permits max 5000 Kč
    | InvestmentSharePercent Int


renderInvestmentShare : InvestmentShare -> String
renderInvestmentShare investmentShare =
    case investmentShare of
        InvestmentSharePercent share ->
            "Investovat maximálně " ++ toString share ++ " % výše úvěru."

        NotSpecified ->
            ""


validate : InvestmentShare -> List String
validate s =
    case s of
        NotSpecified ->
            []

        InvestmentSharePercent pct ->
            Util.validate (pct < 1 || 100 < pct) "Podíl výše úvěru musí být mezi 1 a 100 %"



-- JSON


encode : InvestmentShare -> Value
encode is =
    case is of
        NotSpecified ->
            Encode.list [ Encode.int 1 ]

        InvestmentSharePercent pct ->
            Encode.list [ Encode.int 2, Encode.int pct ]


decoder : Decoder InvestmentShare
decoder =
    Decode.list Decode.int
        |> Decode.andThen
            (\ints ->
                case ints of
                    [ 1 ] ->
                        Decode.succeed NotSpecified

                    [ 2, pct ] ->
                        Decode.succeed <| InvestmentSharePercent pct

                    _ ->
                        Decode.fail <| "Unable to decode InvestmentShare from " ++ toString ints
            )
