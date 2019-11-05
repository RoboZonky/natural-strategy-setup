module Slider exposing (Msg, PercentSlider, init, update, view)

import Html exposing (Html)
import Html.Attributes as Attr exposing (type_)
import Html.Events as Events
import Json.Decode as Decode


type PercentSlider
    = PercentSlider Int


type Msg
    = SetValue Int


init : Int -> PercentSlider
init =
    PercentSlider << clamp 0 100


update : Msg -> PercentSlider -> PercentSlider
update msg (PercentSlider _) =
    case msg of
        SetValue newValue ->
            PercentSlider newValue


view : PercentSlider -> Html Msg
view (PercentSlider value) =
    Html.input
        [ type_ "range"
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
