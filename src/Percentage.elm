module Percentage exposing
    ( Msg
    , Percentage
    , fromInt
    , toInt
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes as Attr exposing (type_)
import Html.Events as Events
import Json.Decode as Decode


type Percentage
    = Percentage Int


type Msg
    = SetValue Int


fromInt : Int -> Percentage
fromInt =
    Percentage << clamp 0 100


toInt : Percentage -> Int
toInt (Percentage value) =
    value


update : Msg -> Percentage -> Percentage
update msg (Percentage _) =
    case msg of
        SetValue newValue ->
            Percentage newValue


view : Percentage -> Html Msg
view (Percentage value) =
    Html.input
        [ type_ "range"
        , Attr.class "percentage"
        , Attr.min "0"
        , Attr.max "100"
        , Attr.step "1"
        , Attr.value (String.fromInt value)
        , Events.on "input"
            (Events.targetValue
                |> Decode.andThen valueDecoder
            )
        ]
        []


valueDecoder : String -> Decode.Decoder Msg
valueDecoder str =
    case String.toInt str of
        Nothing ->
            Decode.fail "ignore"

        Just i ->
            Decode.succeed (SetValue i)
