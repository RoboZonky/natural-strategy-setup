module View.InvestmentShare exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare(..))
import Html exposing (legend, text)
import Html.Attributes as Attr exposing (class, disabled)
import Html.Events exposing (onInput, onSubmit)
import Types exposing (..)
import Util


form : InvestmentShare -> Card.BlockItem Msg
form investmentShare =
    let
        ( isUnrestricted, inputValue ) =
            case investmentShare of
                NotSpecified ->
                    ( True, Input.value defaultValue )

                InvestmentSharePercent pct ->
                    ( False, Input.value <| Util.zeroToEmpty pct )

        validationErrors =
            Util.viewErrors <| InvestmentShare.validate investmentShare
    in
    Card.custom <|
        Form.group []
            [ legend [] [ text "Maximální podíl investice" ]
            , Radio.radio
                [ Radio.checked isUnrestricted
                , Radio.name "investmentShare"
                , Radio.onClick (TargetPortfolioShareChanged "undefined")
                ]
                "Investovat bez ohledu na to jaký podíl výše úvěru moje půjčka pokryje"
            , Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.checked (not isUnrestricted)
                    , Radio.name "investmentShare"
                    , Radio.onClick (TargetPortfolioShareChanged defaultValue)
                    ]
                    "Investovat maximálně"
                , Input.number
                    [ Input.small
                    , inputValue
                    , Input.attrs [ Attr.min "1", Attr.max "100", onInput TargetPortfolioShareChanged, disabled isUnrestricted, class "mx-1" ]
                    ]
                , text "% výše úvěru."
                ]
            , validationErrors
            ]


defaultValue : String
defaultValue =
    "1"
