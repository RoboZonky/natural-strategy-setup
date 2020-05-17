module View.EnumSelect exposing
    ( Config
    , DefaultOptionConfig(..)
    , from
    )

import Dict
import Html exposing (Html)
import Html.Attributes exposing (class, disabled, selected, value)
import Html.Events exposing (stopPropagationOn, targetValue)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)


type alias Config enum msg =
    { -- List of enum values to pick from
      enumValues : List enum

    -- Message to be fired when valid option picked
    , valuePickedMessage : enum -> msg

    --| How to display the value in the dropDown
    , showVisibleLabel : enum -> String
    , defaultOption : DefaultOptionConfig enum
    , enabled : Bool
    }


type DefaultOptionConfig enum
    = DummyOption String --| Text for dummy option informing user to pick something (like "-- select fruit --")
    | DefaultOption enum


from : Config enum msg -> Html msg
from { enumValues, valuePickedMessage, showVisibleLabel, defaultOption, enabled } =
    let
        toOption : Int -> enum -> ( String, Html msg )
        toOption index enum =
            let
                key =
                    String.fromInt index
            in
            ( key
            , Html.option
                [ value key ]
                [ Html.text (showVisibleLabel enum) ]
            )

        toOptionWithDefault : enum -> Int -> enum -> ( String, Html msg )
        toOptionWithDefault defaultVal index enum =
            let
                key =
                    String.fromInt index
            in
            ( key
            , Html.option
                [ value key, selected (enum == defaultVal) ]
                [ Html.text (showVisibleLabel enum) ]
            )

        stringToEnum : String -> Maybe enum
        stringToEnum =
            makeStringToEnum enumValues

        -- Html.Event.onInput modified to only emit event when enum value is successfully decoded from the targetValue string
        enumDecoder : Decoder ( msg, Bool )
        enumDecoder =
            targetValue
                |> Decode.andThen
                    (\str ->
                        case stringToEnum str of
                            Just enumVal ->
                                Decode.succeed ( valuePickedMessage enumVal, True )

                            Nothing ->
                                Decode.fail <| "Failed to decode enum value from " ++ str
                    )

        options : List ( String, Html msg )
        options =
            case defaultOption of
                DummyOption info ->
                    dummyOption info :: List.indexedMap toOption enumValues

                DefaultOption defaultVal ->
                    List.indexedMap (toOptionWithDefault defaultVal) enumValues
    in
    Keyed.node "select"
        [ stopPropagationOn "input" enumDecoder
        , class "form-control"
        , disabled (not enabled)
        ]
        options


dummyOption : String -> ( String, Html msg )
dummyOption info =
    ( "dummyInformativeOption"
    , Html.option
        [ selected True ]
        [ Html.text info ]
    )


{-| Given list of enum values create a lookup function
for converting from enum value's index back to enum's value
-}
makeStringToEnum : List enum -> (String -> Maybe enum)
makeStringToEnum enumValues =
    \str ->
        List.indexedMap (\index val -> ( String.fromInt index, val )) enumValues
            |> Dict.fromList
            |> Dict.get str
