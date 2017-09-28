module View.TargetBalance exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Data.TargetBalance as TargetBalance exposing (TargetBalance(..))
import Html exposing (legend, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onSubmit)
import Types exposing (..)
import Util


form : TargetBalance -> Card.BlockItem Msg
form targetBalance =
    let
        ( isUnspecified, valueAttribute ) =
            case targetBalance of
                NotSpecified ->
                    ( True, Input.value defaultValue )

                TargetBalance val ->
                    ( False, Input.value <| Util.zeroToEmpty val )

        validationErrors =
            Util.viewErrors <| TargetBalance.validate targetBalance
    in
    Card.custom <|
        Form.group []
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
            , validationErrors
            ]


defaultValue : String
defaultValue =
    "200"
