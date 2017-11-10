module Data.TargetBalance
    exposing
        ( TargetBalance(..)
        , decoder
        , defaultTargetBalance
        , encode
        , render
        , validate
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type TargetBalance
    = NotSpecified
    | TargetBalance Int


defaultTargetBalance : TargetBalance
defaultTargetBalance =
    TargetBalance 200


render : TargetBalance -> String
render targetBalance =
    case targetBalance of
        TargetBalance balance ->
            "Investovat pouze pokud disponibilní zůstatek přesáhne " ++ toString balance ++ " Kč."

        NotSpecified ->
            ""


validate : TargetBalance -> List String
validate tb =
    case tb of
        NotSpecified ->
            []

        TargetBalance val ->
            Util.validate (val < 200) "Minimální výše investice na Zonky.cz je 200 Kč. Nastovavat nižší hodnotu nemá smysl."



-- JSON


encode : TargetBalance -> Value
encode tb =
    case tb of
        NotSpecified ->
            Encode.list [ Encode.int 1 ]

        TargetBalance pct ->
            Encode.list [ Encode.int 2, Encode.int pct ]


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
                        Decode.fail <| "Unable to decode TargetBalance from " ++ toString ints
            )
