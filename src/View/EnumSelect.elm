module View.EnumSelect exposing (from)

import Html exposing (Html, option, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Html.Keyed as Keyed


{-| from xs valuePickedMsg noOpMsg infoText toLabel

xs = List of enum values to pick from
valuePickedMsg = message to be fired when valid option picked
noOpMsg = Message to be fired when invalid option picked
infoText = Text for dummy option informing user to pick something (like "-- select fruit --")
toLabel = Function to convert enum value to visible text of select's option

-}
from : List a -> (a -> msg) -> msg -> String -> (a -> String) -> Html msg
from xs valuePickedMsg noOpMsg infoText toLabel =
    let
        toOption x =
            ( -- Html.Keyed needs us to assign unique ID to each keyed node
              toString x
            , option
                [ value (toString x) ]
                [ text (toLabel x) ]
            )

        stringToEnum : String -> Maybe a
        stringToEnum =
            makeStringToEnum xs

        stringToMessage : String -> msg
        stringToMessage str =
            case stringToEnum str of
                Nothing ->
                    noOpMsg

                Just val ->
                    valuePickedMsg val

        informativeOption =
            ( "dummyInformativeOption"
            , option
                [ selected True ]
                [ text infoText ]
            )
    in
    Keyed.node "select"
        [ onInput stringToMessage, class "form-control" ]
        (informativeOption :: List.map toOption xs)


makeStringToEnum : List a -> (String -> Maybe a)
makeStringToEnum xs str =
    List.filter (\x -> toString x == str) xs |> List.head
