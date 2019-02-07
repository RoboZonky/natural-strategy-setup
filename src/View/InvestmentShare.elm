module View.InvestmentShare exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Html exposing (Html, text)
import Html.Events exposing (onSubmit)
import Types exposing (Msg(..))
import Util
import View.NumericInput


form : InvestmentShare -> CardBlock.Item Msg
form investmentShare =
    let
        ( isRestricted, value ) =
            case investmentShare of
                InvestmentShare.NotSpecified ->
                    ( False, defaultValue )

                InvestmentShare.Percent pct ->
                    ( True, Util.zeroToEmpty pct )

        validationErrors =
            Util.viewErrors <| InvestmentShare.validate investmentShare
    in
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ text "Maximální podíl investice" ]
        |> Fieldset.children
            [ Radio.radio
                [ Radio.id "is1"
                , Radio.checked (not isRestricted)
                , Radio.name "investmentShare"
                , Radio.onClick (TargetPortfolioShareChanged "undefined")
                ]
                "Investovat bez ohledu na to jaký podíl výše úvěru moje půjčka pokryje"
            , Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.id "is2"
                    , Radio.checked isRestricted
                    , Radio.name "investmentShare"
                    , Radio.onClick (TargetPortfolioShareChanged defaultValue)
                    ]
                    "Investovat maximálně"
                , numericInput TargetPortfolioShareChanged isRestricted value
                , text "% výše úvěru."
                ]
            , validationErrors
            ]
        |> Fieldset.view
        |> CardBlock.custom


numericInput : (String -> Msg) -> Bool -> String -> Html Msg
numericInput =
    View.NumericInput.numericInput 1 100


defaultValue : String
defaultValue =
    "1"
