module View.TargetPortfolioSize exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Data.TargetPortfolioSize exposing (TargetPortfolioSize(..))
import Html exposing (Html, div, fieldset, input, label, legend, span, text)
import Html.Attributes as Attr exposing (checked, class, disabled, name, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Types exposing (..)


form : TargetPortfolioSize -> Card.BlockItem Msg
form targetPortfolioSize =
    let
        ( isUnbounded, valueAttribute ) =
            case targetPortfolioSize of
                NotSpecified ->
                    ( True, value defaultSize )

                TargetPortfolioSize maxBound ->
                    ( False
                    , if maxBound == 0 then
                        value ""
                      else
                        value (toString maxBound)
                    )
    in
    Card.custom <|
        Form.group []
            [ legend [] [ text "Cílová zůstatková částka" ]
            , Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.checked isUnbounded
                    , Radio.name "portfolioSize"
                    , Radio.onClick (TargetPortfolioSizeChanged "undefined")
                    ]
                    "neomezená"
                , Radio.radio
                    [ Radio.checked (not isUnbounded)
                    , Radio.name "portfolioSize"
                    , Radio.onClick (TargetPortfolioSizeChanged defaultSize)
                    , Radio.attrs [ class "mx-1" ]
                    ]
                    "maximálně"
                , Input.number
                    [ Input.small
                    , Input.onInput TargetPortfolioSizeChanged
                    , Input.disabled isUnbounded
                    , Input.attrs [ Attr.min "0", Attr.max "100000000", valueAttribute, class "mx-1" ]
                    ]
                , text "Kč."
                ]
            ]


defaultSize : String
defaultSize =
    "10000"
