module View.TargetBalance exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Data.TargetBalance exposing (TargetBalance(..))
import Html exposing (Html, div, fieldset, input, label, legend, text)
import Html.Attributes as Attr exposing (checked, disabled, name, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Types exposing (..)


form : TargetBalance -> Card.BlockItem Msg
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
    Card.custom <|
        Form.group [] <|
            [ legend [] [ text "Disponibilní zůstatek" ]
            , Radio.radio
                [ Radio.checked isUnspecified
                , Radio.name "balance"
                , Radio.onClick (TargetBalanceChanged "undefined")
                ]
                "Investovat bez ohledu na disponibilní zůstatek "
            , Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.checked (not isUnspecified)
                    , Radio.name "balance"
                    , Radio.onClick (TargetBalanceChanged defaultValue)
                    ]
                    "Investovat pouze pokud disponibilní zůstatek přesáhne"
                , Input.number
                    [ Input.small
                    , Input.attrs [ Attr.min "200", Attr.max "100000000", onInput TargetBalanceChanged, disabled isUnspecified, valueAttribute ]
                    ]
                , text " Kč."
                ]
            ]
                ++ validationError


defaultValue : String
defaultValue =
    "200"
