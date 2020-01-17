module Data.Migration.Strategy.V1.TargetBalance exposing
    ( TargetBalance(..)
    , decoder
    , render
    )

import Json.Decode as Decode exposing (Decoder)
import Util


type TargetBalance
    = NotSpecified
    | TargetBalance Int


render : TargetBalance -> String
render targetBalance =
    case targetBalance of
        TargetBalance balance ->
            "Investovat pouze pokud disponibilní zůstatek přesáhne " ++ String.fromInt balance ++ " Kč."

        NotSpecified ->
            ""


decoder : Decoder TargetBalance
decoder =
    Decode.list Decode.int
        |> Decode.andThen
            (\ints ->
                case ints of
                    [ 1 ] ->
                        Decode.succeed NotSpecified

                    [ 2, pct ] ->
                        Decode.succeed <| TargetBalance pct

                    _ ->
                        Decode.fail <| "Unable to decode TargetBalance from " ++ Util.intListToString ints
            )
