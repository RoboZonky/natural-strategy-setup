module Main exposing (main)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key)
import Data.Filter as Filters exposing (FilteredItem(..))
import Data.Portfolio
import Data.Strategy as Strategy exposing (StrategyConfiguration)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize(..))
import Data.Tooltip as Tooltip
import Data.VersionedStrategy as VersionedStrategy
import Html exposing (Html, a, footer, h1, text)
import Html.Attributes exposing (class, href, style, target)
import Task
import Time exposing (Posix)
import Types exposing (BaseUrl, CreationModalMsg(..), Msg(..))
import Url exposing (Url)
import Util
import Version
import View.Alert as Alert exposing (AlertData(..))
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
    , generatedOn : Posix
    , baseUrl : BaseUrl
    , alert : AlertData
    }


initialModel : Url -> StrategyConfiguration -> AlertData -> Model
initialModel url strategyConfig initialAlert =
    { strategyConfig = strategyConfig
    , accordionState = Accordion.initialState
    , filterCreationState = FilterCreationModal.init
    , filterDeletionState = FilterDeletionModal.init
    , tooltipStates = Tooltip.initialStates
    , generatedOn = Time.millisToPosix 0
    , baseUrl = Url.toString url
    , alert = initialAlert
    }


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


updateStrategy : (StrategyConfiguration -> StrategyConfiguration) -> Model -> Model
updateStrategy strategyUpdater model =
    { model | strategyConfig = strategyUpdater model.strategyConfig }


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
            updateStrategy (Strategy.setInvestment rating sliderMsg) model

        DefaultPrimaryInvestmentChanged sliderMsg ->
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
                        model.strategyConfig.sellingConfig
                        newModel.strategyConfig.sellingConfig
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
                oldModel.strategyConfig.buyingConfig
                newModel.strategyConfig.buyingConfig
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
    footer [ class "text-center mt-2", style "color" "gray" ]
        [ text "Autor "
        , a
            [ href "http://janhrcek.cz", target "_blank" ]
            [ text "Jan Hrček" ]
        , text ". Běžící verze "
        , a [ href Version.githubCommitLink, target "_blank" ]
            [ text Version.commitHash ]
        , text ". Nahlásit chybu na "
        , a
            [ href "https://github.com/RoboZonky/natural-strategy-setup/issues", target "_blank" ]
            [ text "stránce projektu" ]
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
