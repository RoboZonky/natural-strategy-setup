module View.TargetPortfolioSize exposing (..)

import Data.TargetPortfolioSize exposing (TargetPortfolioSize(..))
import Html exposing (Html, input, label, text, div)
import Html.Attributes as Attr exposing (checked, disabled, name, type_)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)


form : TargetPortfolioSize -> Html Msg
form targetPortfolioSize =
    let
        isUnbounded =
            case targetPortfolioSize of
                Unbounded ->
                    True

                _ ->
                    False
    in
        div []
            [ text "Cílová zůstatková částka je "
            , label []
                [ input [ type_ "radio", name "portfolioSize", onClick (TargetPortfolioSizeChanged "undefined"), checked isUnbounded ] []
                , text " neomezená "
                ]
            , label []
                [ input [ type_ "radio", name "portfolioSize", onClick (TargetPortfolioSizeChanged "0"), checked (not isUnbounded) ] []
                , text " maximalně "
                , input [ type_ "number", Attr.min "0", Attr.max "100000000", onInput TargetPortfolioSizeChanged, disabled isUnbounded ] []
                , text " Kč."
                ]
            ]
