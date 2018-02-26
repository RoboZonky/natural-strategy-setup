module View.TargetBalance exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import Data.TargetBalance as TargetBalance exposing (TargetBalance(NotSpecified, TargetBalance))
import Html exposing (text)
import Html.Attributes as Attr
import Html.Events exposing (onSubmit)
import Types exposing (Msg(NoOp, TargetBalanceChanged))
import Util


form : TargetBalance -> CardBlock.Item Msg
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
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ text "Disponibilní zůstatek" ]
        |> Fieldset.children
            [ Radio.radio
                [ Radio.id "tb1"
                , Radio.checked isUnspecified
                , Radio.name "balance"
                , Radio.onClick (TargetBalanceChanged "undefined")
                ]
                "Investovat bez ohledu na disponibilní zůstatek "
            , Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.id "tb2"
                    , Radio.checked (not isUnspecified)
                    , Radio.name "balance"
                    , Radio.onClick (TargetBalanceChanged defaultValue)
                    ]
                    "Investovat pouze pokud disponibilní zůstatek přesáhne"
                , Input.number
                    [ Input.small
                    , Input.onInput TargetBalanceChanged
                    , Input.disabled isUnspecified
                    , valueAttribute
                    , Input.attrs [ Attr.min defaultValue, Attr.max "100000000", Spacing.mx1 ]
                    ]
                , text " Kč."
                ]
            , validationErrors
            ]
        |> Fieldset.view
        |> CardBlock.custom


defaultValue : String
defaultValue =
    "200"
