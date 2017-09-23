module View.InvestmentShare exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Data.InvestmentShare exposing (InvestmentShare(..))
import Html exposing (div, legend, text)
import Html.Attributes as Attr exposing (class, disabled, style)
import Html.Events exposing (onInput, onSubmit)
import Types exposing (..)
import Util


form : InvestmentShare -> Card.BlockItem Msg
form investmentShare =
    let
        ( isUnrestricted, inputValue, validationError ) =
            case investmentShare of
                NotSpecified ->
                    ( True, Input.value defaultValue, [] )

                InvestmentSharePercent pct ->
                    let
                        validationError =
                            if pct < 1 || 100 < pct then
                                [ div [ style [ ( "color", "red" ) ] ] [ text "Podíl výše úvěru musí být mezi 1 a 100 %" ] ]
                            else
                                []
                    in
                    ( False
                    , Input.value <| Util.zeroToEmpty pct
                    , validationError
                    )
    in
    Card.custom <|
        Form.group [] <|
            [ legend [] [ text "Maximální podíl ivestice" ]
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
            ]
                ++ validationError


defaultValue : String
defaultValue =
    "1"
