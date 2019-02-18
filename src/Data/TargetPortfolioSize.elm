module Data.TargetPortfolioSize exposing
    ( TargetPortfolioSize(..)
    , decoder
    , encode
    , render
    , validate
    )

import Data.Validate as Validate
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type TargetPortfolioSize
    = NotSpecified
    | TargetPortfolioSize Int


render : TargetPortfolioSize -> String
render targetPortfolioSize =
    case targetPortfolioSize of
        TargetPortfolioSize maxBound ->
            "Cílová zůstatková částka je " ++ String.fromInt maxBound ++ " Kč."

        NotSpecified ->
            ""


validate : TargetPortfolioSize -> List String
validate tb =
    case tb of
        NotSpecified ->
            []

        TargetPortfolioSize val ->
            Validate.validate (val < 0) "Cílová zůstatková částka nesmí být záporná."



-- JSON


encode : TargetPortfolioSize -> Value
encode tps =
    case tps of
        NotSpecified ->
            Encode.list Encode.int [ 1 ]

        TargetPortfolioSize x ->
            Encode.list Encode.int [ 2, x ]


decoder : Decoder TargetPortfolioSize
decoder =
    Decode.list Decode.int
        |> Decode.andThen
            (\ints ->
                case ints of
                    [ 2, x ] ->
                        Decode.succeed <| TargetPortfolioSize x

                    [ 1 ] ->
                        Decode.succeed NotSpecified

                    _ ->
                        Decode.fail <| "Unable to decode TargetPortfolioSize from " ++ Util.intListToString ints
            )
