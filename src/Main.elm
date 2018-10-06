module Main exposing (main)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Data.Filter as Filters exposing (FilteredItem(..))
import Data.Investment as Investment
import Data.InvestmentShare as InvestmentShare
import Data.Portfolio
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
import Types exposing (AlertData(..), BaseUrl, CreationModalMsg(ModalTooltipMsg), Msg(..))
import Util
import Version
import View.Alert as Alert
import View.ConfigPreview as ConfigPreview
import View.Filter.CreationModal as FilterCreationModal
import View.Filter.DeletionModal as FilterDeletionModal
import View.Strategy as Strategy


type alias Model =
    { strategyConfig : StrategyConfiguration
    , accordionState : Accordion.State
    , filterCreationState : FilterCreationModal.Model
    , filterDeletionState : FilterDeletionModal.Model
    , tooltipStates : Tooltip.States
    , generatedOn : Date
    , baseUrl : BaseUrl
    , alert : AlertData
    }


initialModel : BaseUrl -> StrategyConfiguration -> AlertData -> Model
initialModel baseUrl strategyConfig initialAlert =
    { strategyConfig = strategyConfig
    , accordionState = Accordion.initialState
    , filterCreationState = FilterCreationModal.init
    , filterDeletionState = FilterDeletionModal.init
    , tooltipStates = Tooltip.initialStates
    , generatedOn = DateTime.date DateTime.epoch
    , baseUrl = baseUrl
    , alert = initialAlert
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
        ( initialStrategy, initialAlert ) =
            loadStrategyFromUrl location

        baseUrl =
            String.split "#" location.href
                |> List.head
                |> Maybe.withDefault "Impossible, as the split will always have 1+ elements"
    in
    ( initialModel baseUrl initialStrategy initialAlert
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
            updateStrategy
                (-- Any change from defaults automatically selects "user defined" portfolio
                 Strategy.setPortfolio Data.Portfolio.Empty << Strategy.setPortfolioShareRange rating sliderMsg
                )
                model

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

        DismisAlert ->
            { model | alert = NoAlert }

        NoOp ->
            model


{-| Return a function which will either leave filters in the strategy as they are, or rever them to what they were before
-}
userDecisionToFilterUpdater : FilterDeletionModal.UserDecision -> StrategyConfiguration -> StrategyConfiguration
userDecisionToFilterUpdater userDecision =
    case userDecision of
        FilterDeletionModal.RestorePreviousBuying previousBuyingConfig ->
            Strategy.setBuyingConfiguration previousBuyingConfig

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
                FilterDeletionModal.init
            else
                FilterDeletionModal.askForConfirmation
                    (FilterDeletionModal.BuyingConfigChange
                        oldModel.strategyConfig.buyingConfig
                        newModel.strategyConfig.buyingConfig
                    )
    in
    { newModel | filterDeletionState = newFilterDeletionState }


wrapIntOrEmpty : (Int -> a) -> a -> String -> a
wrapIntOrEmpty intWrapper emptyVal str =
    Util.emptyToZero str |> String.toInt |> Result.map intWrapper |> Result.withDefault emptyVal


view : Model -> Html Msg
view { strategyConfig, accordionState, filterCreationState, filterDeletionState, tooltipStates, generatedOn, baseUrl, alert } =
    Grid.containerFluid []
        [ h1 [] [ text "Konfigurace strategie" ]
        , Alert.view alert
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


loadStrategyFromUrl : Navigation.Location -> ( StrategyConfiguration, AlertData )
loadStrategyFromUrl location =
    if String.isEmpty location.hash then
        ( Strategy.defaultStrategyConfiguration, NoAlert )
    else
        case Strategy.strategyFromUrlHash (String.dropLeft 1 {- drop '#' -} location.hash) of
            Ok strategy ->
                ( strategy, SuccessAlert "Strategie byla úspěšně načtena z URL" )

            Err e ->
                ( Strategy.defaultStrategyConfiguration
                , ErrorAlert ("Při pokusu obnovit strategii z URL " ++ location.href ++ "\n\n došlo k chybě: " ++ e)
                )
