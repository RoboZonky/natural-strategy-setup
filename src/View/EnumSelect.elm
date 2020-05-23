module View.EnumSelect exposing
    ( Config
    , from
    , fromGrouped
    )

import Dict
import Html exposing (Html)
import Html.Attributes exposing (class, disabled, hidden, selected, value)
import Html.Events exposing (stopPropagationOn, targetValue)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List


type alias Config enum msg r =
    { r
        | -- List of enum values to pick from
          enumValues : List enum

        -- Message to be fired when valid option picked
        , valuePickedMessage : enum -> msg

        --| How to display the value in the dropDown
        , optionLabel : enum -> String
        , enabled : Bool
    }


from : Config enum msg { defaultOption : Maybe enum } -> Html msg
from { enumValues, valuePickedMessage, optionLabel, defaultOption, enabled } =
    let
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

        toOptionWithDefault : Int -> enum -> ( String, Html msg )
        toOptionWithDefault index enum =
            let
                key =
                    String.fromInt index
            in
            ( key
            , Html.option
                (value key
                    :: (case defaultOption of
                            Just def ->
                                [ selected (enum == def) ]

                            Nothing ->
                                []
                       )
                )
                [ Html.text (optionLabel enum) ]
            )

        addDummyOptionWhenNoDefault =
            case defaultOption of
                Nothing ->
                    (::)
                        ( "dummyInformativeOption"
                        , dummyOption ""
                        )

                Just _ ->
                    identity
    in
    Keyed.node "select"
        [ stopPropagationOn "input" enumDecoder
        , class "form-control"
        , disabled (not enabled)
        ]
        (addDummyOptionWhenNoDefault <|
            List.indexedMap toOptionWithDefault enumValues
        )


fromGrouped : Config enum msg { dummyLabel : String, groupLabel : enum -> String } -> Html msg
fromGrouped { enumValues, valuePickedMessage, optionLabel, dummyLabel, enabled, groupLabel } =
    let
        stringToEnum : String -> Maybe enum
        stringToEnum =
            makeStringToEnum enumValues

        groupedOptions : List ( ( Int, enum ), List ( Int, enum ) )
        groupedOptions =
            enumValues
                |> List.indexedMap Tuple.pair
                |> List.groupWhile (\( _, e1 ) ( _, e2 ) -> groupLabel e1 == groupLabel e2)

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

        options : List (Html msg)
        options =
            dummyOption dummyLabel
                :: List.map
                    (\( ( ix, e ), rest ) ->
                        Html.optgroup [ Html.Attributes.attribute "label" (groupLabel e) ] <|
                            List.map
                                (\( index, enum ) ->
                                    Html.option
                                        [ value (String.fromInt index) ]
                                        [ Html.text (optionLabel enum) ]
                                )
                                (( ix, e ) :: rest)
                    )
                    groupedOptions
    in
    Html.select
        [ stopPropagationOn "input" enumDecoder
        , class "form-control"
        , disabled (not enabled)
        ]
        options


dummyOption : String -> Html msg
dummyOption text =
    Html.option [ selected True, disabled True, hidden True ]
        [ Html.text text ]


{-| Given list of enum values create a lookup function
for converting from enum value's index back to enum's value
-}
makeStringToEnum : List enum -> (String -> Maybe enum)
makeStringToEnum enumValues =
    \str ->
        List.indexedMap (\index val -> ( String.fromInt index, val )) enumValues
            |> Dict.fromList
            |> Dict.get str
