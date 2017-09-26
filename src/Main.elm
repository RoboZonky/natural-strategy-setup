module Main exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Data.Filter exposing (FilteredItem(Participation_To_Sell), getFilteredItem)
import Data.Investment as Investment
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare(..))
import Data.Strategy exposing (..)
import Data.TargetBalance as TargetBalance exposing (TargetBalance(TargetBalance))
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (..)
import Data.Tooltip as Tooltip
import Html exposing (Html, a, footer, h1, text)
import Html.Attributes exposing (class, href, style)
import Slider exposing (SliderStates)
import Types exposing (..)
import Util
import Version
import View.ConfigPreview as ConfigPreview
import View.Filter.FilterCreationModal as FilterCreationModal
import View.Strategy as Strategy


type alias Model =
    { strategyConfig : StrategyConfiguration
    , accordionState : Accordion.State
    , filterCreationState : FilterCreationModal.Model
    , tooltipStates : Tooltip.States
    , sliderStates : SliderStates
    }


initialModel : Model
initialModel =
    { strategyConfig = defaultStrategyConfiguration
    , accordionState = Accordion.initialState
    , filterCreationState = FilterCreationModal.initialState
    , tooltipStates = Tooltip.initialStates
    , sliderStates = Slider.initialSliderStates
    }


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions { sliderStates, strategyConfig } =
    Sub.batch
        [ Slider.sliderChangeSubscription sliderStates
        , Investment.investmentSlidersSubscriptions strategyConfig.investmentSizeOverrides
        , Investment.defaultInvestmentSliderSubscription strategyConfig.generalSettings.defaultInvestmentSize
        ]


updateStrategy : (StrategyConfiguration -> StrategyConfiguration) -> Model -> Model
updateStrategy strategyUpdater model =
    { model | strategyConfig = strategyUpdater model.strategyConfig }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelper msg model, Cmd.none )


updateHelper : Msg -> Model -> Model
updateHelper msg model =
    case msg of
        PortfolioChanged portfolio ->
            updateStrategy (setPortfolio portfolio) { model | sliderStates = Slider.initialSliderStates }

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

        ChangePortfolioSharePercentage rating sliderMsg ->
            let
                newSliderStates =
                    Slider.updateSliders rating sliderMsg model.sliderStates

                newRange =
                    Slider.getSliderRangeFor rating newSliderStates

                newModel =
                    updateStrategy (setPortfolioShareRange rating newRange) model
            in
            { newModel | sliderStates = newSliderStates }

        ChangeInvestment rating sliderMsg ->
            updateStrategy (setInvestment rating sliderMsg) model

        ChangeDefaultInvestment sliderMsg ->
            updateStrategy (setDefaultInvestment sliderMsg) model

        RemoveBuyFilter index ->
            updateStrategy (removeBuyFilter index) model

        RemoveSellFilter index ->
            updateStrategy (removeSellFilter index) model

        AccordionMsg state ->
            { model | accordionState = state }

        TooltipMsg tipId tooltipState ->
            { model | tooltipStates = Tooltip.update tipId tooltipState model.tooltipStates }

        ModalMsg (ModalTooltipMsg tipId tooltipState) ->
            { model | tooltipStates = Tooltip.update tipId tooltipState model.tooltipStates }

        ModalMsg msg ->
            let
                ( filterCreationState, maybeNewFilter ) =
                    FilterCreationModal.update msg model.filterCreationState

                strategyUpdater =
                    case maybeNewFilter of
                        Nothing ->
                            identity

                        Just newFilter ->
                            case getFilteredItem newFilter of
                                Participation_To_Sell ->
                                    addSellFilter newFilter

                                _ ->
                                    addBuyFilter newFilter
            in
            { model
                | filterCreationState = filterCreationState
                , strategyConfig = strategyUpdater model.strategyConfig
            }

        NoOp ->
            model


updateStrategyIfValidInt : String -> (Int -> StrategyConfiguration) -> StrategyConfiguration -> StrategyConfiguration
updateStrategyIfValidInt intStr strategyUpdater strategyConfig =
    intStr
        |> Util.emptyToZero
        |> String.toInt
        |> Result.map (\parsedInt -> strategyUpdater parsedInt)
        |> Result.withDefault strategyConfig


view : Model -> Html Msg
view { strategyConfig, accordionState, filterCreationState, tooltipStates, sliderStates } =
    Grid.containerFluid []
        [ h1 [] [ text "Konfigurace strategie" ]
        , Grid.row []
            [ Strategy.form strategyConfig accordionState filterCreationState tooltipStates sliderStates
            , ConfigPreview.view strategyConfig
            ]
        , infoFooter
        ]


infoFooter : Html Msg
infoFooter =
    footer [ class "text-center mt-2", style [ ( "color", "gray" ) ] ]
        [ text "Autor "
        , a [ href "http://janhrcek.cz" ] [ text "Jan Hrček" ]
        , text ". Běžící verze "
        , a [ href Version.githubCommitLink ] [ text Version.commitHash ]
        , text ". Nahlásit chybu na "
        , a [ href "https://github.com/RoboZonky/natural-strategy-setup/issues" ] [ text "stránce projektu" ]
        ]
