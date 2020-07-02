module Percentage exposing
    ( Msg(..)
    , Percentage
    , fromFloat
    , fromInt
    , toFloat
    , toInt
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes as Attr exposing (type_)
import Html.Events as Events
import Json.Decode as Decode


{-| Internally storing as Float, because predefined "conservative" portfolio has .5 values
and we want to sum them up correctly without rounding
-}
type Percentage
    = Percentage Float


type Msg
    = SetValue Int


fromFloat : Float -> Percentage
fromFloat =
    clamp 0 100 >> Percentage


toFloat : Percentage -> Float
toFloat (Percentage value) =
    value


fromInt : Int -> Percentage
fromInt =
    Basics.toFloat >> fromFloat


toInt : Percentage -> Int
toInt =
    toFloat >> Basics.round


update : Msg -> Percentage -> Percentage
update msg _ =
    case msg of
        SetValue newValue ->
            Percentage (Basics.toFloat newValue)


view : Percentage -> Html Msg
view percentage =
    Html.input
        [ type_ "range"
        , Attr.class "percentage"
        , Attr.min "0"
        , Attr.max "100"
        , Attr.step "1"
        , Attr.value <| String.fromInt <| toInt percentage
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
