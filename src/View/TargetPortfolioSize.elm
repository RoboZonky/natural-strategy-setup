module View.TargetPortfolioSize exposing (form)

import Data.TargetPortfolioSize exposing (TargetPortfolioSize(..))
import Html exposing (Html, div, fieldset, input, label, legend, text)
import Html.Attributes as Attr exposing (checked, disabled, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)


form : TargetPortfolioSize -> Html Msg
form targetPortfolioSize =
    let
        ( isUnbounded, valueAttribute ) =
            case targetPortfolioSize of
                Unbounded ->
                    ( True, value defaultSize )

                Bounded val ->
                    ( False, value (toString val) )
    in
    fieldset []
        [ legend [] [ text "Cílová zůstatková částka je " ]
        , label []
            [ input [ type_ "radio", name "portfolioSize", onClick (TargetPortfolioSizeChanged "undefined"), checked isUnbounded ] []
            , text " neomezená "
            ]
        , label []
            [ input [ type_ "radio", name "portfolioSize", onClick (TargetPortfolioSizeChanged defaultSize), checked (not isUnbounded) ] []
            , text " maximalně "
            , input [ type_ "number", Attr.min "0", Attr.max "100000000", onInput TargetPortfolioSizeChanged, disabled isUnbounded, valueAttribute ] []
            , text " Kč."
            ]
        ]


defaultSize : String
defaultSize =
    "10000"
