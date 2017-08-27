module View.TargetPortfolioSize exposing (form)

import Bootstrap.Card as Card
import Data.TargetPortfolioSize exposing (TargetPortfolioSize(..))
import Html exposing (Html, div, fieldset, input, label, legend, span, text)
import Html.Attributes as Attr exposing (checked, disabled, name, type_, value)
import Html.Events exposing (onClick, onInput)
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
        fieldset []
            [ legend [] [ text "Cílová zůstatková částka je " ]
            , span []
                [ input [ type_ "radio", name "portfolioSize", onClick (TargetPortfolioSizeChanged "undefined"), checked isUnbounded ] []
                , text " neomezená "
                ]
            , span []
                [ input [ type_ "radio", name "portfolioSize", onClick (TargetPortfolioSizeChanged defaultSize), checked (not isUnbounded) ] []
                , text " maximálně "
                , input [ type_ "number", Attr.min "0", Attr.max "100000000", onInput TargetPortfolioSizeChanged, disabled isUnbounded, valueAttribute ] []
                , text " Kč."
                ]
            ]


defaultSize : String
defaultSize =
    "10000"
