module Main exposing (..)

import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare(..))
import Data.Strategy exposing (..)
import Data.TargetBalance as TargetBalance exposing (TargetBalance(TargetBalance))
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (..)
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
                    emptyStringToZero targetSizeStr
                        |> String.toInt
                        |> Result.map TargetPortfolioSize
                        |> Result.withDefault TargetPortfolioSize.NotSpecified
            in
            setTargetPortfolioSize targetSize model

        TargetPortfolioShareChanged shareStr ->
            let
                share =
                    emptyStringToZero shareStr
                        |> String.toInt
                        |> Result.map InvestmentSharePercent
                        |> Result.withDefault InvestmentShare.NotSpecified
            in
            setDefaultInvestmentShare share model

        TargetBalanceChanged newBalanceStr ->
            let
                newBalance =
                    emptyStringToZero newBalanceStr
                        |> String.toInt
                        |> Result.map TargetBalance
                        |> Result.withDefault TargetBalance.NotSpecified
            in
            setTargetBalance newBalance model

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

        AddBuyFilter newFilter ->
            addBuyFilter newFilter model

        RemoveBuyFilter index ->
            removeBuyFilter index model



-- This is to make radio + input subforms (TargetPortfolioSize, InvestmentShare and TargetBalance)
-- NOT switch back to NotSpecified when user deletes the whole input
-- TODO probably introduce 2 separate messages to control radios vs inputs


emptyStringToZero : String -> String
emptyStringToZero str =
    if String.isEmpty str then
        "0"
    else
        str


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
