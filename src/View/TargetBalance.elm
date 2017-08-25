module View.TargetBalance exposing (form)

import Data.TargetBalance exposing (TargetBalance(..))
import Html exposing (Html, div, fieldset, input, label, legend, text)
import Html.Attributes as Attr exposing (checked, disabled, name, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)


form : TargetBalance -> Html Msg
form targetBalance =
    let
        ( isUnspecified, valueAttribute, validationError ) =
            case targetBalance of
                NotSpecified ->
                    ( True, value defaultValue, [] )

                TargetBalance val ->
                    let
                        validationError =
                            if val < 200 then
                                [ div [ style [ ( "color", "red" ) ] ]
                                    [ text "Minimální výše investice na Zonky.cz je 200 Kč. Nastovavat nižší hodnotu nemá smysl." ]
                                ]
                            else
                                []
                    in
                    ( False
                    , if val == 0 then
                        value ""
                      else
                        value (toString val)
                    , validationError
                    )
    in
    fieldset [] <|
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
            ++ validationError


defaultValue : String
defaultValue =
    "200"
