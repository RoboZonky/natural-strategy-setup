module View.TargetBalance exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Data.TargetBalance exposing (TargetBalance(..))
import Html exposing (Html, div, legend, text)
import Html.Attributes as Attr exposing (class, style)
import Html.Events exposing (onSubmit)
import Types exposing (..)
import Util


form : TargetBalance -> Card.BlockItem Msg
form targetBalance =
    let
        ( isUnspecified, valueAttribute, validationError ) =
            case targetBalance of
                NotSpecified ->
                    ( True, Input.value defaultValue, [] )

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
                    , Input.value <| Util.zeroToEmpty val
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
                    , Input.onInput TargetBalanceChanged
                    , Input.disabled isUnspecified
                    , valueAttribute
                    , Input.attrs [ Attr.min defaultValue, Attr.max "100000000", class "mx-1" ]
                    ]
                , text " Kč."
                ]
            ]
                ++ validationError


defaultValue : String
defaultValue =
    "200"
