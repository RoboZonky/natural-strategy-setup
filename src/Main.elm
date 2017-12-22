module Main exposing (main)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Data.Filter exposing (FilteredItem(Participation_To_Sell), getFilteredItem)
import Data.Investment as Investment
import Data.InvestmentShare as InvestmentShare
import Data.PortfolioStructure as PortfolioStructure
import Data.Strategy as Strategy exposing (StrategyConfiguration)
import Data.TargetBalance as TargetBalance exposing (TargetBalance(TargetBalance))
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (..)
import Data.Tooltip as Tooltip
import Html exposing (Html, a, footer, h1, text)
import Html.Attributes exposing (class, href, style)
import Task
import Time
import Time.DateTime as DateTime exposing (DateTime)
import Types exposing (ModalMsg(ModalTooltipMsg), Msg(..))
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
    , generatedOn : DateTime
    }


initialModel : Model
initialModel =
    { strategyConfig = Strategy.defaultStrategyConfiguration
    , accordionState = Accordion.initialState
    , filterCreationState = FilterCreationModal.init
    , tooltipStates = Tooltip.initialStates
    , generatedOn = DateTime.epoch
    }


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Task.perform SetDateTime Time.now )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions { strategyConfig } =
    Sub.batch
        [ PortfolioStructure.portfolioSlidersSubscription strategyConfig.portfolioShares
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
            updateStrategy (Strategy.setPortfolio portfolio) model

        TargetPortfolioSizeChanged targetSizeStr ->
            let
                targetSize =
                    wrapIntOrEmpty TargetPortfolioSize TargetPortfolioSize.NotSpecified targetSizeStr
            in
            updateStrategy (Strategy.setTargetPortfolioSize targetSize) model

        TargetPortfolioShareChanged shareStr ->
            let
                share =
                    wrapIntOrEmpty InvestmentShare.Percent InvestmentShare.NotSpecified shareStr
            in
            updateStrategy (Strategy.setDefaultInvestmentShare share) model

        TargetBalanceChanged newBalanceStr ->
            let
                newBalance =
                    wrapIntOrEmpty TargetBalance TargetBalance.NotSpecified newBalanceStr
            in
            updateStrategy (Strategy.setTargetBalance newBalance) model

        ConfirmationFormMsg confMsg ->
            updateStrategy (Strategy.updateNotificationSettings confMsg) model

        ChangePortfolioSharePercentage rating sliderMsg ->
            updateStrategy (Strategy.setPortfolioShareRange rating sliderMsg) model

        ChangeInvestment rating sliderMsg ->
            updateStrategy (Strategy.setInvestment rating sliderMsg) model

        ChangeDefaultInvestment sliderMsg ->
            updateStrategy (Strategy.setDefaultInvestment sliderMsg) model

        RemoveBuyFilter index ->
            updateStrategy (Strategy.removeBuyFilter index) model

        RemoveSellFilter index ->
            updateStrategy (Strategy.removeSellFilter index) model

        AccordionMsg state ->
            { model | accordionState = state }

        TooltipMsg tipId tooltipState ->
            { model | tooltipStates = Tooltip.update tipId tooltipState model.tooltipStates }

        ModalMsg (ModalTooltipMsg tipId tooltipState) ->
            { model | tooltipStates = Tooltip.update tipId tooltipState model.tooltipStates }

        ModalMsg modalMsg ->
            let
                ( filterCreationState, maybeNewFilter ) =
                    FilterCreationModal.update modalMsg model.filterCreationState

                strategyUpdater =
                    case maybeNewFilter of
                        Nothing ->
                            identity

                        Just newFilter ->
                            case getFilteredItem newFilter of
                                Participation_To_Sell ->
                                    Strategy.addSellFilter newFilter

                                _ ->
                                    Strategy.addBuyFilter newFilter
            in
            { model
                | filterCreationState = filterCreationState
                , strategyConfig = strategyUpdater model.strategyConfig
            }

        SetDateTime timestamp ->
            { model | generatedOn = DateTime.fromTimestamp timestamp }

        ShareStrategy ->
            -- TODO generate encoded-obfuscated strategy containing URL and show it in UI
            -- let  _ = Debug.log "Strategy JSON: " <| Json.Encode.encode 0 <| Strategy.encodeStrategy model.strategyConfig
            -- in
            model

        NoOp ->
            model


wrapIntOrEmpty : (Int -> a) -> a -> String -> a
wrapIntOrEmpty intWrapper emptyVal str =
    Util.emptyToZero str |> String.toInt |> Result.map intWrapper |> Result.withDefault emptyVal


view : Model -> Html Msg
view { strategyConfig, accordionState, filterCreationState, tooltipStates, generatedOn } =
    Grid.containerFluid []
        [ h1 [] [ text "Konfigurace strategie" ]
        , Grid.row []
            [ Strategy.form strategyConfig accordionState filterCreationState tooltipStates
            , ConfigPreview.view generatedOn strategyConfig
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
