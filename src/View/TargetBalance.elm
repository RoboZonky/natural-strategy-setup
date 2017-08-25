module View.TargetBalance exposing (form)

import Data.TargetBalance exposing (TargetBalance(..))
import Html exposing (Html, div, fieldset, input, label, legend, text)
import Html.Attributes as Attr exposing (checked, disabled, name, type_)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)


form : TargetBalance -> Html Msg
form targetBalance =
    let
        isUnspecified =
            case targetBalance of
                Unspecified ->
                    True

                _ ->
                    False
    in
    fieldset []
        [ legend [] [ text "Disponibilní zůstatek" ]
        , text "Investovat "
        , div []
            [ label []
                [ input [ type_ "radio", name "portfolioSize", onClick (TargetBalanceChanged "undefined"), checked isUnspecified ] []
                , text " bez ohledu na disponibilní zůstatek "
                ]
            ]
        , div []
            [ label []
                [ input [ type_ "radio", name "portfolioSize", onClick (TargetBalanceChanged "200"), checked (not isUnspecified) ] []
                , text " pouze pokud disponibilní zůstatek přesáhne "
                , input [ type_ "number", Attr.min "0", Attr.max "100000000", onInput TargetBalanceChanged, disabled isUnspecified ] []
                , text " Kč."
                ]
            ]
        ]
