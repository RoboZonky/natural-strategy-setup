module Main exposing (..)

import Data.Strategy exposing (..)
import Data.TargetPortfolioSize exposing (..)
import Html exposing (Html, text)
import Types exposing (..)
import View.ConfigPreview as ConfigPreview
import View.StrategyForm as StrategyForm


type alias Model =
    ParsedStrategy


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

        ChangePortfolioShareMin rating newMinStr ->
            String.toInt newMinStr
                |> Result.map (\newMin -> setPortfolioShareMin rating newMin model)
                |> Result.withDefault model

        ChangePortfolioShareMax rating newMaxStr ->
            String.toInt newMaxStr
                |> Result.map (\newMax -> setPortfolioShareMax rating newMax model)
                |> Result.withDefault model

        TargetPortfolioSizeChanged targetSizeStr ->
            let
                targetSize =
                    String.toInt targetSizeStr |> Result.map Bounded |> Result.withDefault Unbounded
            in
                setTargetPortfolioSize targetSize model


view : Model -> Html Msg
view model =
    Html.div []
        [ StrategyForm.view model
        , ConfigPreview.view model
        ]
