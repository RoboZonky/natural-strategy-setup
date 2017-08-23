module Main exposing (..)

import Data.InvestmentShare exposing (InvestmentShare(..))
import Data.Strategy exposing (..)
import Data.TargetPortfolioSize exposing (..)
import Html exposing (Html, text)
import Types exposing (..)
import View.ConfigPreview as ConfigPreview
import View.Strategy as Strategy


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

        ToggleNotificationOnRating rating isEnabled ->
            setNotification rating isEnabled model

        ChangeInvestmentMin rating newMinStr ->
            String.toInt newMinStr
                |> Result.map (\newMin -> setInvestmentMin rating newMin model)
                |> Result.withDefault model

        ChangeInvestmentMax rating newMaxStr ->
            String.toInt newMaxStr
                |> Result.map (\newMax -> setInvestmentMax rating newMax model)
                |> Result.withDefault model

        ChangeDefaultInvestmentMin newMinStr ->
            String.toInt newMinStr
                |> Result.map (\newMin -> setDefaultInvestmentMin newMin model)
                |> Result.withDefault model

        ChangeDefaultInvestmentMax newMaxStr ->
            String.toInt newMaxStr
                |> Result.map (\newMax -> setDefaultInvestmentMax newMax model)
                |> Result.withDefault model

        TargetPortfolioShareChanged shareStr ->
            let
                share =
                    String.toInt shareStr |> Result.map PercentShare |> Result.withDefault Unrestricted
            in
            setDefaultInvestmentShare share model


view : Model -> Html Msg
view model =
    Html.div []
        [ Strategy.form model
        , ConfigPreview.view model
        ]
