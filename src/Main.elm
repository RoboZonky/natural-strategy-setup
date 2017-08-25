module Main exposing (..)

import Data.InvestmentShare exposing (InvestmentShare(..))
import Data.Strategy exposing (..)
import Data.TargetBalance exposing (TargetBalance(..))
import Data.TargetPortfolioSize exposing (..)
import Html exposing (Html, text)
import Types exposing (..)
import View.ConfigPreview as ConfigPreview
import View.Strategy as Strategy


type alias Model =
    StrategyConfiguration


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = defaultStrategyConfiguration
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PortfolioChanged portfolio ->
            setPortfolio portfolio model

        TargetPortfolioSizeChanged targetSizeStr ->
            let
                targetSize =
                    String.toInt targetSizeStr |> Result.map Bounded |> Result.withDefault Unbounded
            in
            setTargetPortfolioSize targetSize model

        TargetPortfolioShareChanged shareStr ->
            let
                share =
                    String.toInt shareStr |> Result.map PercentShare |> Result.withDefault Unrestricted
            in
            setDefaultInvestmentShare share model

        ToggleNotificationOnRating rating isEnabled ->
            setNotification rating isEnabled model

        ChangePortfolioShareMin rating newMinStr ->
            updateModelIfValidInt newMinStr (\newMin -> setPortfolioShareMin rating newMin model) model

        ChangePortfolioShareMax rating newMaxStr ->
            updateModelIfValidInt newMaxStr (\newMax -> setPortfolioShareMax rating newMax model) model

        ChangeInvestmentMin rating newMinStr ->
            updateModelIfValidInt newMinStr (\newMin -> setInvestmentMin rating newMin model) model

        ChangeInvestmentMax rating newMaxStr ->
            updateModelIfValidInt newMaxStr (\newMax -> setInvestmentMax rating newMax model) model

        ChangeDefaultInvestmentMin newMinStr ->
            updateModelIfValidInt newMinStr (\newMin -> setDefaultInvestmentMin newMin model) model

        ChangeDefaultInvestmentMax newMaxStr ->
            updateModelIfValidInt newMaxStr (\newMax -> setDefaultInvestmentMax newMax model) model

        TargetBalanceChanged newBalanceStr ->
            let
                newBalance =
                    String.toInt newBalanceStr |> Result.map TargetBalance |> Result.withDefault Unspecified
            in
            setTargetBalance newBalance model


updateModelIfValidInt : String -> (Int -> Model) -> Model -> Model
updateModelIfValidInt intStr modelUpdater model =
    String.toInt intStr
        |> Result.map (\parsedInt -> modelUpdater parsedInt)
        |> Result.withDefault model


view : Model -> Html Msg
view model =
    Html.div []
        [ Strategy.form model
        , ConfigPreview.view model
        ]
