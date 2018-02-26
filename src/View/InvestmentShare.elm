module View.InvestmentShare exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Html exposing (text)
import Html.Attributes as Attr exposing (disabled)
import Html.Events exposing (onInput, onSubmit)
import Types exposing (Msg(NoOp, TargetPortfolioShareChanged))
import Util


form : InvestmentShare -> CardBlock.Item Msg
form investmentShare =
    let
        ( isUnrestricted, inputValue ) =
            case investmentShare of
                InvestmentShare.NotSpecified ->
                    ( True, Input.value defaultValue )

                InvestmentShare.Percent pct ->
                    ( False, Input.value <| Util.zeroToEmpty pct )

        validationErrors =
            Util.viewErrors <| InvestmentShare.validate investmentShare
    in
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ text "Maximální podíl investice" ]
        |> Fieldset.children
            [ Radio.radio
                [ Radio.id "is1"
                , Radio.checked isUnrestricted
                , Radio.name "investmentShare"
                , Radio.onClick (TargetPortfolioShareChanged "undefined")
                ]
                "Investovat bez ohledu na to jaký podíl výše úvěru moje půjčka pokryje"
            , Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.id "is2"
                    , Radio.checked (not isUnrestricted)
                    , Radio.name "investmentShare"
                    , Radio.onClick (TargetPortfolioShareChanged defaultValue)
                    ]
                    "Investovat maximálně"
                , Input.number
                    [ Input.small
                    , inputValue
                    , Input.attrs [ Attr.min "1", Attr.max "100", onInput TargetPortfolioShareChanged, disabled isUnrestricted, Spacing.mx1 ]
                    ]
                , text "% výše úvěru."
                ]
            , validationErrors
            ]
        |> Fieldset.view
        |> CardBlock.custom


defaultValue : String
defaultValue =
    "1"
