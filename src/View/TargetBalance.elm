module View.TargetBalance exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Data.TargetBalance as TargetBalance exposing (TargetBalance(NotSpecified, TargetBalance))
import Html exposing (Html, text)
import Html.Events exposing (onSubmit)
import Types exposing (Msg(NoOp, TargetBalanceChanged))
import Util
import View.NumericInput


form : TargetBalance -> CardBlock.Item Msg
form targetBalance =
    let
        ( isSpecified, value ) =
            case targetBalance of
                NotSpecified ->
                    ( False, defaultValue )

                TargetBalance val ->
                    ( True, Util.zeroToEmpty val )

        validationErrors =
            Util.viewErrors <| TargetBalance.validate targetBalance
    in
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ text "Disponibilní zůstatek" ]
        |> Fieldset.children
            [ Radio.radio
                [ Radio.id "tb1"
                , Radio.checked (not isSpecified)
                , Radio.name "balance"
                , Radio.onClick (TargetBalanceChanged "undefined")
                ]
                "Investovat bez ohledu na disponibilní zůstatek "
            , Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.id "tb2"
                    , Radio.checked isSpecified
                    , Radio.name "balance"
                    , Radio.onClick (TargetBalanceChanged defaultValue)
                    ]
                    "Investovat pouze pokud disponibilní zůstatek přesáhne"
                , numericInput TargetBalanceChanged isSpecified value
                , text " Kč."
                ]
            , validationErrors
            ]
        |> Fieldset.view
        |> CardBlock.custom


numericInput : (String -> Msg) -> Bool -> String -> Html Msg
numericInput =
    View.NumericInput.numericInput 200 100000000


defaultValue : String
defaultValue =
    "200"
