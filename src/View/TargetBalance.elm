module View.TargetBalance exposing (form)

import Data.TargetBalance exposing (TargetBalance(..))
import Html exposing (Html, div, fieldset, input, label, legend, text)
import Html.Attributes as Attr exposing (checked, disabled, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)


form : TargetBalance -> Html Msg
form targetBalance =
    let
        ( isUnspecified, valueAttribute ) =
            case targetBalance of
                Unspecified ->
                    ( True, value defaultValue )

                TargetBalance val ->
                    ( False, value (toString val) )
    in
    fieldset []
        [ legend [] [ text "Disponibilní zůstatek" ]
        , text "Investovat "
        , div []
            [ label []
                [ input [ type_ "radio", name "balance", onClick (TargetBalanceChanged "undefined"), checked isUnspecified ] []
                , text " bez ohledu na disponibilní zůstatek "
                ]
            ]
        , div []
            [ label []
                [ input [ type_ "radio", name "balance", onClick (TargetBalanceChanged defaultValue), checked (not isUnspecified) ] []
                , text " pouze pokud disponibilní zůstatek přesáhne "
                , input [ type_ "number", Attr.min "200", Attr.max "100000000", onInput TargetBalanceChanged, disabled isUnspecified, valueAttribute ] []
                , text " Kč."
                ]
            ]
        ]


defaultValue : String
defaultValue =
    "200"
