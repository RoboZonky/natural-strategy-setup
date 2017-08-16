module Main exposing (..)

import Html exposing (Html, text)
import Data.Strategy exposing (..)
import Data.TargetPortfolioSize exposing (..)
import Types exposing (..)
import View.StrategyForm as StrategyForm
import View.ConfigPreview as ConfigPreview


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = defaultSimpleStrategy
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SimpleStrategySelected ->
            defaultSimpleStrategy

        ComplexStrategySelected ->
            defaultComplexStrategy

        PortfolioChanged portfolio ->
            setPortfolio portfolio model

        TargetPortfolioSizeChanged targetSizeStr ->
            let
                targetSize =
                    case String.toInt targetSizeStr of
                        Ok sz ->
                            Bounded sz

                        Err error ->
                            Unbounded
            in
                setTargetPortfolioSize targetSize model


view : Model -> Html Msg
view model =
    Html.div []
        [ StrategyForm.view model
        , ConfigPreview.view model
        ]
