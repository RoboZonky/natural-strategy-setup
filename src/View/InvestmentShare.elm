module View.InvestmentShare exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Data.InvestmentShare exposing (InvestmentShare(..))
import Html exposing (Html, div, fieldset, input, label, legend, text)
import Html.Attributes as Attr exposing (checked, class, disabled, name, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Types exposing (..)


form : InvestmentShare -> Card.BlockItem Msg
form investmentShare =
    let
        ( isUnrestricted, valueAttribute, validationError ) =
            case investmentShare of
                NotSpecified ->
                    ( True, value defaultValue, [] )

                InvestmentSharePercent pct ->
                    let
                        validationError =
                            if pct < 1 || 100 < pct then
                                [ div [ style [ ( "color", "red" ) ] ] [ text "Podíl výše úvěru musí být mezi 1 a 100 %" ] ]
                            else
                                []
                    in
                    ( False
                    , if pct == 0 then
                        value ""
                      else
                        value (toString pct)
                    , validationError
                    )
    in
    Card.custom <|
        Form.group [] <|
            [ legend [] [ text "Maximální podíl ivestice" ]
            , Radio.radio
                [ Radio.checked isUnrestricted
                , Radio.name "portfolioSize"
                , Radio.onClick (TargetPortfolioShareChanged "undefined")
                ]
                "Investovat bez ohledu na to jaký podíl výše úvěru moje půjčka pokryje"
            , Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.checked (not isUnrestricted)
                    , Radio.name "portfolioSize"
                    , Radio.onClick (TargetPortfolioShareChanged defaultValue)
                    ]
                    "Investovat maximálně"
                , Input.number
                    [ Input.small
                    , Input.attrs [ Attr.min "1", Attr.max "100", onInput TargetPortfolioShareChanged, disabled isUnrestricted, valueAttribute, class "mx-1" ]
                    ]
                , text "% výše úvěru."
                ]
            ]
                ++ validationError


defaultValue : String
defaultValue =
    "1"
