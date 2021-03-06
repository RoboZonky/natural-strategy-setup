module Data.ReservationSetting exposing
    ( ReservationSetting(..)
    , decoder
    , defaultSetting
    , encode
    , render
    )

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Util


type ReservationSetting
    = -- Not showing this in the UI as this is experimental and could swarm
      -- the reservation system with robots taking over the system, rendering it useles for people.
      -- FullOwnership
      AcceptMatching
    | Ignore


defaultSetting : ReservationSetting
defaultSetting =
    Ignore


allSettings : List ReservationSetting
allSettings =
    [ --, FullOwnership
      AcceptMatching
    , Ignore
    ]


render : ReservationSetting -> String
render rss =
    case rss of
        -- FullOwnership ->
        --     "Robot má převzít kontrolu nad rezervačním systémem a přijímat rezervace půjček odpovídajících této strategii."
        AcceptMatching ->
            "Robot má pravidelně kontrolovat rezervační systém a přijímat rezervace půjček odpovídajících této strategii."

        Ignore ->
            "Robot má zcela ignorovat rezervační systém."



-- JSON


encode : ReservationSetting -> Value
encode =
    Util.enumEncoder allSettings


decoder : Decoder ReservationSetting
decoder =
    Util.enumDecoder "ReservationSetting" allSettings
