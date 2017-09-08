module Main exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare(..))
import Data.Strategy exposing (..)
import Data.TargetBalance as TargetBalance exposing (TargetBalance(TargetBalance))
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (..)
import Html exposing (Html, h1, text)
import Types exposing (..)
import Util
import View.ConfigPreview as ConfigPreview
import View.Filter.FilterCreationModal as FilterCreationModal
import View.Strategy as Strategy


type alias Model =
    { strategyConfig : StrategyConfiguration
    , accordionState : Accordion.State
    , filterCreationState : FilterCreationModal.State
    }


initialModel : Model
initialModel =
    { strategyConfig = defaultStrategyConfiguration
    , accordionState = Accordion.initialState
    , filterCreationState = FilterCreationModal.initialState
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }


updateStrategy : (StrategyConfiguration -> StrategyConfiguration) -> Model -> Model
updateStrategy strategyUpdater model =
    { model | strategyConfig = strategyUpdater model.strategyConfig }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PortfolioChanged portfolio ->
            updateStrategy (setPortfolio portfolio) model

        TargetPortfolioSizeChanged targetSizeStr ->
            let
                targetSize =
                    Util.emptyToZero targetSizeStr
                        |> String.toInt
                        |> Result.map TargetPortfolioSize
                        |> Result.withDefault TargetPortfolioSize.NotSpecified
            in
            updateStrategy (setTargetPortfolioSize targetSize) model

        TargetPortfolioShareChanged shareStr ->
            let
                share =
                    Util.emptyToZero shareStr
                        |> String.toInt
                        |> Result.map InvestmentSharePercent
                        |> Result.withDefault InvestmentShare.NotSpecified
            in
            updateStrategy (setDefaultInvestmentShare share) model

        TargetBalanceChanged newBalanceStr ->
            let
                newBalance =
                    Util.emptyToZero newBalanceStr
                        |> String.toInt
                        |> Result.map TargetBalance
                        |> Result.withDefault TargetBalance.NotSpecified
            in
            updateStrategy (setTargetBalance newBalance) model

        ConfirmationFormMsg msg ->
            updateStrategy (updateNotificationSettings msg) model

        ChangePortfolioShareMin rating newMinStr ->
            updateStrategy (updateStrategyIfValidInt newMinStr (\newMin -> setPortfolioShareMin rating newMin model.strategyConfig)) model

        ChangePortfolioShareMax rating newMaxStr ->
            updateStrategy (updateStrategyIfValidInt newMaxStr (\newMax -> setPortfolioShareMax rating newMax model.strategyConfig)) model

        ChangeInvestmentMin rating newMinStr ->
            updateStrategy (updateStrategyIfValidInt newMinStr (\newMin -> setInvestmentMin rating newMin model.strategyConfig)) model

        ChangeInvestmentMax rating newMaxStr ->
            updateStrategy (updateStrategyIfValidInt newMaxStr (\newMax -> setInvestmentMax rating newMax model.strategyConfig)) model

        ChangeDefaultInvestmentMin newMinStr ->
            updateStrategy (updateStrategyIfValidInt newMinStr (\newMin -> setDefaultInvestmentMin newMin model.strategyConfig)) model

        ChangeDefaultInvestmentMax newMaxStr ->
            updateStrategy (updateStrategyIfValidInt newMaxStr (\newMax -> setDefaultInvestmentMax newMax model.strategyConfig)) model

        AddBuyFilter newFilter ->
            updateStrategy (addBuyFilter newFilter) model

        RemoveBuyFilter index ->
            updateStrategy (removeBuyFilter index) model

        AccordionMsg state ->
            { model | accordionState = state }

        ModalMsg msg ->
            { model | filterCreationState = FilterCreationModal.update msg model.filterCreationState }

        NoOp ->
            model


updateStrategyIfValidInt : String -> (Int -> StrategyConfiguration) -> StrategyConfiguration -> StrategyConfiguration
updateStrategyIfValidInt intStr strategyUpdater strategyConfig =
    String.toInt intStr
        |> Result.map (\parsedInt -> strategyUpdater parsedInt)
        |> Result.withDefault strategyConfig


view : Model -> Html Msg
view { strategyConfig, accordionState, filterCreationState } =
    Grid.containerFluid []
        [ h1 [] [ text "Konfigurace strategie" ]
        , Grid.row []
            [ Strategy.form strategyConfig accordionState filterCreationState
            , ConfigPreview.view strategyConfig
            ]
        ]
