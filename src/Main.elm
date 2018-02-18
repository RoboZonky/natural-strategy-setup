module Main exposing (main)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Data.Filter as Filters exposing (FilteredItem(Participation_To_Sell))
import Data.Investment as Investment
import Data.InvestmentShare as InvestmentShare
import Data.PortfolioStructure as PortfolioStructure
import Data.Strategy as Strategy exposing (StrategyConfiguration)
import Data.TargetBalance as TargetBalance exposing (TargetBalance(TargetBalance))
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize(TargetPortfolioSize))
import Data.Tooltip as Tooltip
import Html exposing (Html, a, footer, h1, text)
import Html.Attributes exposing (class, href, style)
import Navigation
import Task
import Time
import Time.Date exposing (Date)
import Time.DateTime as DateTime
import Types exposing (BaseUrl, CreationModalMsg(ModalTooltipMsg), Msg(..))
import Util
import Version
import View.ConfigPreview as ConfigPreview
import View.Filter.FilterCreationModal as FilterCreationModal
import View.Filter.FilterDeletionModal as FilterDeletionModal
import View.Strategy as Strategy


type alias Model =
    { strategyConfig : StrategyConfiguration
    , accordionState : Accordion.State
    , filterCreationState : FilterCreationModal.Model
    , filterDeletionState : FilterDeletionModal.Model
    , tooltipStates : Tooltip.States
    , generatedOn : Date
    , baseUrl : BaseUrl
    }


initialModel : BaseUrl -> StrategyConfiguration -> Model
initialModel baseUrl strategyConfig =
    { strategyConfig = strategyConfig
    , accordionState = Accordion.initialState
    , filterCreationState = FilterCreationModal.init
    , filterDeletionState = FilterDeletionModal.initClosed
    , tooltipStates = Tooltip.initialStates
    , generatedOn = DateTime.date DateTime.epoch
    , baseUrl = baseUrl
    }


main : Program Never Model Msg
main =
    Navigation.program (always NoOp)
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        initialStrategy =
            loadStrategyFromUrl location
                |> Maybe.withDefault Strategy.defaultStrategyConfiguration

        baseUrl =
            String.split "#" location.href
                |> List.head
                |> Maybe.withDefault "Impossible, as the split will always have 1+ elements"
    in
    ( initialModel baseUrl initialStrategy
    , Cmd.batch
        [ Task.perform SetDateTime Time.now

        {- clear the hash from URL -}
        , Navigation.newUrl baseUrl
        ]
    )


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

        ExitConfigChanged exitConfig ->
            updateStrategy (Strategy.setExitConfig exitConfig) model

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

        CreationModalMsg (ModalTooltipMsg tipId tooltipState) ->
            { model | tooltipStates = Tooltip.update tipId tooltipState model.tooltipStates }

        CreationModalMsg modalMsg ->
            let
                ( newFilterCreationState, maybeNewFilter ) =
                    FilterCreationModal.update modalMsg model.filterCreationState

                maybeInsertFilter =
                    maybeNewFilter
                        |> Maybe.map marketplaceFilterToFilterUpdater
                        |> Maybe.withDefault identity
            in
            updateStrategy maybeInsertFilter { model | filterCreationState = newFilterCreationState }

        DeletionModalMsg modalMsg ->
            let
                ( newFilterDeletionState, maybeUserDecision ) =
                    FilterDeletionModal.update modalMsg model.filterDeletionState

                maybeRestoreFilters =
                    maybeUserDecision
                        |> Maybe.map userDecisionToFilterUpdater
                        |> Maybe.withDefault identity
            in
            updateStrategy maybeRestoreFilters { model | filterDeletionState = newFilterDeletionState }

        SetDateTime timestamp ->
            { model | generatedOn = DateTime.date <| DateTime.fromTimestamp timestamp }

        SetBuyingConfiguration buyConf ->
            let
                newModel =
                    updateStrategy (Strategy.setBuyConf buyConf) model
            in
            askForBuyFilterDeletionConfirmation model newModel

        TogglePrimaryMarket enable ->
            let
                newModel =
                    updateStrategy (Strategy.togglePrimaryMarket enable) model
            in
            askForBuyFilterDeletionConfirmation model newModel

        ToggleSecondaryMarket enable ->
            let
                newModel =
                    updateStrategy (Strategy.toggleSecondaryMarket enable) model
            in
            askForBuyFilterDeletionConfirmation model newModel

        SetSellingConfiguration sellingConfiguration ->
            let
                newModel =
                    updateStrategy (Strategy.setSellConf sellingConfiguration) model
            in
            askForSellFilterDeletionConfirmation model newModel

        NoOp ->
            model


{-| Return a function which will either leave filters in the strategy as they are, or rever them to what they were before
-}
userDecisionToFilterUpdater : FilterDeletionModal.UserDecision -> StrategyConfiguration -> StrategyConfiguration
userDecisionToFilterUpdater userDecision =
    case userDecision of
        FilterDeletionModal.RestorePreviousBuying previousBuyingConfig ->
            Strategy.setBuyingConfiguration previousBuyingConfig

        FilterDeletionModal.RestorePreviousSelling previousSellingConfig ->
            Strategy.setSellingConfiguration previousSellingConfig

        FilterDeletionModal.OkToDelete ->
            identity


{-| Return a function that will insert given filter into the right place of StrategyConfiguration
-}
marketplaceFilterToFilterUpdater : Filters.MarketplaceFilter -> StrategyConfiguration -> StrategyConfiguration
marketplaceFilterToFilterUpdater newFilter =
    case newFilter.whatToFilter of
        Participation_To_Sell ->
            Strategy.addSellFilter newFilter

        _ ->
            Strategy.addBuyFilter newFilter


askForBuyFilterDeletionConfirmation : Model -> Model -> Model
askForBuyFilterDeletionConfirmation oldModel newModel =
    let
        removedFilters =
            Filters.getFiltersRemovedByBuyingConfigurationChange
                oldModel.strategyConfig.buyingConfig
                newModel.strategyConfig.buyingConfig

        newFilterDeletionState =
            if List.isEmpty removedFilters then
                FilterDeletionModal.initClosed
            else
                FilterDeletionModal.askForConfirmation
                    (FilterDeletionModal.BuyingConfigChange
                        oldModel.strategyConfig.buyingConfig
                        newModel.strategyConfig.buyingConfig
                    )
    in
    { newModel | filterDeletionState = newFilterDeletionState }


askForSellFilterDeletionConfirmation : Model -> Model -> Model
askForSellFilterDeletionConfirmation oldModel newModel =
    let
        removedFilters =
            Filters.getFiltersRemovedBySellingConfigurationChange
                oldModel.strategyConfig.sellingConfig
                newModel.strategyConfig.sellingConfig

        newFilterDeletionState =
            if List.isEmpty removedFilters then
                FilterDeletionModal.initClosed
            else
                FilterDeletionModal.askForConfirmation
                    (FilterDeletionModal.SellingConfigChange
                        oldModel.strategyConfig.sellingConfig
                        newModel.strategyConfig.sellingConfig
                    )
    in
    { newModel | filterDeletionState = newFilterDeletionState }


wrapIntOrEmpty : (Int -> a) -> a -> String -> a
wrapIntOrEmpty intWrapper emptyVal str =
    Util.emptyToZero str |> String.toInt |> Result.map intWrapper |> Result.withDefault emptyVal


view : Model -> Html Msg
view { strategyConfig, accordionState, filterCreationState, filterDeletionState, tooltipStates, generatedOn, baseUrl } =
    Grid.containerFluid []
        [ h1 [] [ text "Konfigurace strategie" ]
        , Grid.row []
            [ Strategy.form strategyConfig accordionState filterCreationState filterDeletionState tooltipStates generatedOn
            , ConfigPreview.view baseUrl generatedOn strategyConfig
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


loadStrategyFromUrl : Navigation.Location -> Maybe StrategyConfiguration
loadStrategyFromUrl location =
    let
        base64EncodedStrategyJson =
            String.dropLeft 1 {- drop '#' -} location.hash
    in
    case Strategy.strategyFromUrlHash base64EncodedStrategyJson of
        Ok strategy ->
            Just strategy

        Err err ->
            Debug.log ("Failed to load strategy from URL " ++ location.href ++ " - The error was " ++ err)
                Nothing
