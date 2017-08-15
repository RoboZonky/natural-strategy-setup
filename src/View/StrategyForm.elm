module View.StrategyForm exposing (..)

import Data.Strategy exposing (..)
import Data.Strategy.Portfolio as Portfolio exposing (DefaultPortfolio(..))
import Html exposing (Html, div, h2, input, label, option, select, text, textarea)
import Html.Attributes as Attr exposing (checked, cols, disabled, height, name, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)
import View.TargetPortfolioSize


view : Model -> Html Msg
view model =
    let
        ( isSimple, subform ) =
            case model of
                SimpleStrategy defaultPortfolio ->
                    ( True, defaultPortfolioForm )

                ComplexStrategy params ->
                    ( False, complexStrategyFormView params )
    in
        div []
            [ h2 [] [ text "Konfigurace strategie" ]
            , label []
                [ input [ type_ "radio", name "strategyRadio", onClick SimpleStrategySelected, checked isSimple ] []
                , text "Jednoduchá"
                ]
            , label []
                [ input [ type_ "radio", name "strategyRadio", onClick ComplexStrategySelected, checked (not isSimple) ] []
                , text "Pokročilá"
                ]
            , subform
            ]


defaultPortfolioForm : Html Msg
defaultPortfolioForm =
    div [] [ text "Robot má udržovat ", defaultPortfolioSelect, text " portfolio." ]


complexStrategyFormView : ComplexStrategyParameters -> Html Msg
complexStrategyFormView parameters =
    div []
        [ h2 [] [ text "Obecná nastavení" ]
        , defaultPortfolioForm
        , View.TargetPortfolioSize.form parameters.generalSettings.targetPortfolioSize
        ]


defaultPortfolioSelect : Html Msg
defaultPortfolioSelect =
    select [ onInput (PortfolioChanged << Portfolio.fromString) ] <|
        List.map
            (\portfolio ->
                option
                    [ value (toString portfolio) ]
                    [ text (Portfolio.toString portfolio) ]
            )
            [ Conservative, Balanced, Progressive, Empty ]
