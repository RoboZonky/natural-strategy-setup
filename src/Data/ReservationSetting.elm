module Data.ReservationSetting exposing
    ( ReservationSetting(..)
    , allSettings
    , decoder
    , encode
    , render
    )

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Util


type ReservationSetting
    = AcceptMatching
    | FullOwnership
    | Ignore


allSettings : List ReservationSetting
allSettings =
    [ AcceptMatching
    , FullOwnership
    , Ignore
    ]


render : ReservationSetting -> String
render rss =
    case rss of
        AcceptMatching ->
            "Robot má pravidelně kontrolovat rezervační systém a přijímat rezervace půjček odpovídajících této strategii."

        FullOwnership ->
            "Robot má převzít kontrolu nad rezervačním systémem a přijímat rezervace půjček odpovídajících této strategii."

        Ignore ->
            "Robot má zcela ignorovat rezervační systém."



-- JSON


encode : ReservationSetting -> Value
encode =
    Util.enumEncoder allSettings


decoder : Decoder ReservationSetting
decoder =
    Util.enumDecoder "ReservationSetting" allSettings
