module Data.Migration.Strategy.V2.Confirmation exposing
    ( ConfirmationSettings(..)
    , decoder
    , defaultSettings
    )

import Json.Decode as Decode exposing (Decoder)


type ConfirmationSettings
    = NoConfirmation
    | Confirm


defaultSettings : ConfirmationSettings
defaultSettings =
    NoConfirmation


decoder : Decoder ConfirmationSettings
decoder =
    Decode.field "a" Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    0 ->
                        Decode.succeed NoConfirmation

                    1 ->
                        Decode.succeed Confirm

                    other ->
                        Decode.fail <| "Failed to decode ConfirmationSettings. Was expecting 0 or 1, but got " ++ String.fromInt other
            )
