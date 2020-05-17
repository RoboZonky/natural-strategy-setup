module Main exposing (main)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Bootstrap.Popover as Popover
import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key)
import Data.ExitConfig as ExitConfig
import Data.Filter as Filters exposing (BuyingConfiguration, FilteredItem(..), SellingConfiguration)
import Data.Filter.Conditions.Rating exposing (Rating)
import Data.Investment as Investment
import Data.Portfolio exposing (Portfolio)
import Data.ReservationSetting exposing (ReservationSetting)
import Data.Strategy as Strategy exposing (BaseUrl, StrategyConfiguration)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize(..))
import Data.Tooltip as Tooltip exposing (TipId)
import Data.VersionedStrategy as VersionedStrategy
import Html exposing (Html)
import Html.Attributes exposing (class, href, style, target)
import Percentage
import Task
import Time exposing (Posix)
import Url exposing (Url)
import Util
import Version
import View.Alert as Alert exposing (AlertData(..))
import View.ConfigPreview as ConfigPreview
import View.Filter.CreationModal as FilterCreationModal
import View.Filter.DeletionModal as FilterDeletionModal
import View.Strategy as Strategy
import View.Tooltip as Tooltip


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = viewDocument
        , subscriptions = \_ -> Sub.none
        , onUrlChange = always NoOp
        , onUrlRequest = LoadUrl
        }


type alias Model =
    { strategyConfiguration : StrategyConfiguration
    , accordionState : Accordion.State
    , filterCreationState : FilterCreationModal.Model
    , filterDeletionState : FilterDeletionModal.Model
    , tooltipStates : Tooltip.States
    , generatedOn : Posix
    , baseUrl : BaseUrl
    , alert : AlertData
    }


initialModel : Url -> StrategyConfiguration -> AlertData -> Model
initialModel url strategyConfiguration initialAlert =
    { strategyConfiguration = strategyConfiguration
    , accordionState = Accordion.initialState
    , filterCreationState = FilterCreationModal.init
    , filterDeletionState = FilterDeletionModal.init
    , tooltipStates = Tooltip.initialStates
    , generatedOn = Time.millisToPosix 0
    , baseUrl = Url.toString url
    , alert = initialAlert
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    let
        ( initialStrategy, initialAlert ) =
            loadStrategyFromUrl url

        baseUrl =
            {- clear the fragment from URL -}
            { url | fragment = Nothing }
    in
    ( initialModel baseUrl initialStrategy initialAlert
    , Cmd.batch
        [ Task.perform SetDateTime Time.now
        , Browser.Navigation.replaceUrl key (Url.toString baseUrl)
        ]
    )


type Msg
    = PortfolioChanged Portfolio
    | ExitConfigChanged ExitConfig.ExitConfig
    | TargetPortfolioSizeChanged String
    | PortfolioPercentageChanged Rating Percentage.Msg
    | DefaultPrimaryInvestmentChanged Investment.Msg
    | DefaultSecondaryPurchaseChanged Investment.Msg
    | PrimaryInvestmentChanged Rating Investment.Msg
    | SecondaryPurchaseChanged Rating Investment.Msg
    | ReservationSettingChanged ReservationSetting
    | RemoveBuyFilter Int
    | RemoveSellFilter Int
    | SellingConfigChanged SellingConfiguration
    | SetSellingConfig SellingConfiguration
    | SetBuyingConfig BuyingConfiguration
    | TogglePrimaryMarket Bool
    | ToggleSecondaryMarket Bool
    | AccordionMsg Accordion.State
    | CreationModalMsg FilterCreationModal.Msg
    | TooltipMsg TipId Popover.State
    | SetDateTime Posix
    | DismissAlert
    | LoadUrl Browser.UrlRequest
    | NoOp


updateStrategy : (StrategyConfiguration -> StrategyConfiguration) -> Model -> Model
updateStrategy strategyUpdater model =
    { model | strategyConfiguration = strategyUpdater model.strategyConfiguration }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadUrl urlRequest ->
            -- The only message with nontrivial command.
            -- Needed to enable navigation to external Help pages etc.
            ( model, loadExternalUrl urlRequest )

        _ ->
            ( updateHelper msg model, Cmd.none )


loadExternalUrl : UrlRequest -> Cmd a
loadExternalUrl urlRequest =
    case urlRequest of
        External externalUrl ->
            Browser.Navigation.load externalUrl

        Internal _ ->
            Cmd.none


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

        PortfolioPercentageChanged rating sliderMsg ->
            updateStrategy
                -- Any change from defaults automatically selects "user defined" portfolio
                (Strategy.setPortfolio Data.Portfolio.UserDefined << Strategy.setPortfolioSharePercentage rating sliderMsg)
                model

        PrimaryInvestmentChanged rating sliderMsg ->
            updateStrategy (Strategy.setInvestmentPrimary rating sliderMsg) model

        SecondaryPurchaseChanged rating sliderMsg ->
            updateStrategy (Strategy.setInvestmentSecondary rating sliderMsg) model

        DefaultPrimaryInvestmentChanged sliderMsg ->
            updateStrategy (Strategy.setDefaultInvestmentPrimary sliderMsg) model

        DefaultSecondaryPurchaseChanged sliderMsg ->
            updateStrategy (Strategy.setDefaultInvestmentSecondary sliderMsg) model

        RemoveBuyFilter index ->
            updateStrategy (Strategy.removeBuyFilter index) model

        RemoveSellFilter index ->
            updateStrategy (Strategy.removeSellFilter index) model

        AccordionMsg state ->
            { model | accordionState = state }

        TooltipMsg tipId tooltipState ->
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

        SetDateTime posix ->
            { model | generatedOn = posix }

        ReservationSettingChanged reservationSetting ->
            updateStrategy (Strategy.setReservationSetting reservationSetting) model

        SellingConfigChanged sellingConfig ->
            let
                newModel =
                    updateStrategy (Strategy.setSellingConfiguration sellingConfig) model
            in
            { newModel
                | filterDeletionState =
                    FilterDeletionModal.confirmSellFiltersDeletion
                        model.strategyConfiguration.sellingConfig
                        newModel.strategyConfiguration.sellingConfig
            }

        SetBuyingConfig buyingConfig ->
            updateStrategy (Strategy.setBuyingConfiguration buyingConfig) model
                |> closeDeletionConfirmationModal

        SetSellingConfig sellingConfig ->
            updateStrategy (Strategy.setSellingConfiguration sellingConfig) model
                |> closeDeletionConfirmationModal

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

        DismissAlert ->
            { model | alert = NoAlert }

        LoadUrl _ ->
            -- already handled in update
            model

        NoOp ->
            model


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
    { newModel
        | filterDeletionState =
            FilterDeletionModal.confirmBuyFiltersDeletion
                oldModel.strategyConfiguration.buyingConfig
                newModel.strategyConfiguration.buyingConfig
    }


closeDeletionConfirmationModal :
    { a | filterDeletionState : FilterDeletionModal.Model }
    -> { a | filterDeletionState : FilterDeletionModal.Model }
closeDeletionConfirmationModal record =
    { record | filterDeletionState = FilterDeletionModal.init }


wrapIntOrEmpty : (Int -> a) -> a -> String -> a
wrapIntOrEmpty intWrapper emptyVal str =
    Util.emptyToZero str |> String.toInt |> Maybe.map intWrapper |> Maybe.withDefault emptyVal


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "Robozonky :: Konfigurace Strategie"
    , body = [ view model ]
    }


view : Model -> Html Msg
view { strategyConfiguration, accordionState, filterCreationState, filterDeletionState, tooltipStates, generatedOn, baseUrl, alert } =
    Grid.containerFluid []
        [ Html.h1 [] [ Html.text "Konfigurace strategie" ]
        , Alert.view DismissAlert alert
        , Grid.row []
            [ Strategy.form strategyConfig
                strategyConfiguration
                accordionState
                filterCreationState
                filterDeletionState
                tooltipStates
                generatedOn
            , ConfigPreview.view baseUrl generatedOn strategyConfiguration
            ]
        , infoFooter
        ]


infoFooter : Html Msg
infoFooter =
    Html.footer
        [ class "fixed-bottom text-center py-3 container"
        , style "color" "gray"
        ]
        [ Html.text "Autor "
        , Html.a
            [ href "http://janhrcek.cz", target "_blank" ]
            [ Html.text "Jan Hrček" ]
        , Html.text ". Běžící verze "
        , Html.a [ href Version.githubCommitLink, target "_blank" ]
            [ Html.text Version.commitHash ]
        , Html.text ". Nahlásit chybu na "
        , Html.a
            [ href "https://github.com/RoboZonky/natural-strategy-setup/issues", target "_blank" ]
            [ Html.text "stránce projektu" ]
        ]


loadStrategyFromUrl : Url -> ( StrategyConfiguration, AlertData )
loadStrategyFromUrl url =
    case url.fragment of
        Nothing ->
            ( Strategy.defaultStrategyConfiguration, NoAlert )

        Just hash ->
            case VersionedStrategy.loadStrategy hash of
                Ok ( strategy, migrationWarnings ) ->
                    if List.isEmpty migrationWarnings then
                        ( strategy, SuccessAlert "Strategie byla úspěšně načtena z URL" )

                    else
                        ( strategy, WarningAlert migrationWarnings )

                Err e ->
                    ( Strategy.defaultStrategyConfiguration
                    , ErrorAlert ("Při pokusu obnovit strategii z URL " ++ Url.toString url ++ "\n\n došlo k chybě: " ++ e)
                    )


strategyConfig : Strategy.Config Msg
strategyConfig =
    { accordionMsg = AccordionMsg
    , defaultPrimaryInvestmentChanged = DefaultPrimaryInvestmentChanged
    , defaultSecondaryPurchaseChanged = DefaultSecondaryPurchaseChanged
    , primaryInvestmentChanged = PrimaryInvestmentChanged
    , secondaryPurchaseChanged = SecondaryPurchaseChanged
    , filterDeletionModalConfig =
        { setBuyingConfig = SetBuyingConfig
        , setSellingConfig = SetSellingConfig
        }
    , filterCreationModalConfig =
        { msg = CreationModalMsg
        , tooltipConfig = tooltipConfig
        }
    , portfolioStructureConfig =
        { portfolioPercentageChanged = PortfolioPercentageChanged
        , portfolioChanged = PortfolioChanged
        , tooltipConfig = tooltipConfig
        , noOp = NoOp
        }
    , buyingConfigConfig =
        { tooltipConfig = tooltipConfig
        , removeBuyFilter = RemoveBuyFilter
        , togglePrimaryMarket = TogglePrimaryMarket
        , toggleSecondaryMarket = ToggleSecondaryMarket
        , openCreationModal = \complexity filters -> CreationModalMsg <| FilterCreationModal.OpenCreationModal complexity filters
        }
    , sellingConfigConfig =
        { tooltipConfig = tooltipConfig
        , sellingConfigChanged = SellingConfigChanged
        , removeSellFilter = RemoveSellFilter
        , openCreationModal = \complexity filters -> CreationModalMsg <| FilterCreationModal.OpenCreationModal complexity filters
        , noOp = NoOp
        }
    , targetPortfolioSizeConfig =
        { targetPortfolioSizeChanged = TargetPortfolioSizeChanged
        , noOp = NoOp
        }
    , reservationSettingChanged = ReservationSettingChanged
    , exitConfigConfig =
        { exitConfigChanged = ExitConfigChanged
        , tooltipConfig = tooltipConfig
        , noOp = NoOp
        }
    , noOp = NoOp
    }


tooltipConfig : Tooltip.Config Msg
tooltipConfig =
    { tooltipMsg = TooltipMsg
    , noOp = NoOp
    }
