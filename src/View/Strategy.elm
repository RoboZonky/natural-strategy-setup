module View.Strategy exposing (Config, form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Filter as Filter
import Data.Filter.Conditions.Rating exposing (Rating)
import Data.Investment as Investment
import Data.ReservationSetting exposing (ReservationSetting)
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration)
import Data.Tooltip as Tooltip
import Html exposing (Html)
import Time exposing (Posix)
import View.BuyingConfig as BuyingConfig
import View.CardHeightWorkaround exposing (markOpenedAccordionCard)
import View.ExitConfig as ExitConfig
import View.Filter.CreationModal as FilterCreationModal
import View.Filter.DeletionModal as FilterDeletionModal
import View.PortfolioStructure as PortfolioStructure
import View.ReservationSetting as ReservationSetting
import View.SellConfig as SellConfig
import View.TargetPortfolioSize as TargetPortfolioSize


type alias Config msg =
    { accordionMsg : Accordion.State -> msg
    , defaultPrimaryInvestmentChanged : Investment.Msg -> msg
    , defaultSecondaryPurchaseChanged : Investment.Msg -> msg
    , primaryInvestmentChanged : Rating -> Investment.Msg -> msg
    , secondaryPurchaseChanged : Rating -> Investment.Msg -> msg
    , reservationSettingChanged : ReservationSetting -> msg
    , portfolioStructureConfig : PortfolioStructure.Config msg
    , buyingConfigConfig : BuyingConfig.Config msg
    , sellingConfigConfig : SellConfig.Config msg
    , targetPortfolioSizeConfig : TargetPortfolioSize.Config msg
    , exitConfigConfig : ExitConfig.Config msg
    , filterCreationModalConfig : FilterCreationModal.Config msg
    , filterDeletionModalConfig : FilterDeletionModal.Config msg
    , noOp : msg
    }


form :
    Config msg
    -> StrategyConfiguration
    -> Accordion.State
    -> FilterCreationModal.Model
    -> FilterDeletionModal.Model
    -> Tooltip.States
    -> Posix
    -> Grid.Column msg
form config strategyConfiguration accordionState filterCreationState filterDeletionState tooltipStates generatedOn =
    Grid.col
        [ Col.xs6 ]
        [ strategyForm config strategyConfiguration accordionState tooltipStates generatedOn
        , FilterCreationModal.view config.filterCreationModalConfig filterCreationState tooltipStates
        , FilterDeletionModal.view config.filterDeletionModalConfig filterDeletionState
        ]


strategyForm : Config msg -> StrategyConfiguration -> Accordion.State -> Tooltip.States -> Posix -> Html msg
strategyForm config { generalSettings, portfolioStructure, primaryInvestmentOverrides, secondaryPurchaseOverrides, buyingConfig, sellingConfig } accordionState tooltipStates generatedOn =
    Accordion.config config.accordionMsg
        |> Accordion.onlyOneOpen
        |> Accordion.cards
            [ generalSettingsCard config generalSettings accordionState tooltipStates generatedOn
            , PortfolioStructure.form config.portfolioStructureConfig generalSettings.portfolio portfolioStructure accordionState tooltipStates
            , Investment.form (primaryInvestmentConfig config)
                (Filter.isBuyingOnPrimaryEnabled buyingConfig)
                generalSettings.defaultPrimaryInvestmentSize
                primaryInvestmentOverrides
            , Investment.form (secondaryPurchaseConfig config)
                (Filter.isBuyingOnSecondaryEnabled buyingConfig)
                generalSettings.defaultSecondaryPurchaseSize
                secondaryPurchaseOverrides
            , BuyingConfig.form config.buyingConfigConfig buyingConfig accordionState tooltipStates
            , SellConfig.form config.sellingConfigConfig sellingConfig accordionState tooltipStates
            ]
        |> Accordion.view accordionState


primaryInvestmentConfig : Config msg -> Investment.Config msg
primaryInvestmentConfig config =
    { accordionId = "primaryInvestment"
    , accordionHeader = "Primární tržiště - výše investice"
    , marketplaceNotEnabled =
        Html.div []
            [ Html.text "Toto nastavení není k dispozici, protože jste v sekci "
            , Html.b [] [ Html.text "Pravidla nákupu" ]
            , Html.text " zaškrtli možnost "
            , Html.b [] [ Html.text "Ignorovat všechny půjčky" ]
            ]
    , defaultLabel = "Běžná výše investice je "
    , overrideLabel = "Pokud si přejete, aby se výše investice do půjček lišily od běžné výše na základě rizikových kategorií, upravte je pomocí posuvníků"
    , onDefaultInvestmentChange = config.defaultPrimaryInvestmentChanged
    , onInvestmentChange = config.primaryInvestmentChanged
    , noOp = config.noOp
    }


secondaryPurchaseConfig : Config msg -> Investment.Config msg
secondaryPurchaseConfig config =
    { accordionId = "secondaryPurchase"
    , accordionHeader = "Sekundární tržiště - výše nákupu"
    , marketplaceNotEnabled =
        Html.div []
            [ Html.text "Toto nastavení není k dispozici, protože jste v sekci "
            , Html.b [] [ Html.text "Pravidla nákupu" ]
            , Html.text " zaškrtli možnost "
            , Html.b [] [ Html.text "Ignorovat všechny participace" ]
            ]
    , defaultLabel = "Běžná částka pro nákup participace je maximálně "
    , overrideLabel = "Pokud si přejete, aby se maximální výše nákupu lišily od běžné výše na základě rizikových kategorií, upravte je pomocí posuvníků"
    , onDefaultInvestmentChange = config.defaultSecondaryPurchaseChanged
    , onInvestmentChange = config.secondaryPurchaseChanged
    , noOp = config.noOp
    }


generalSettingsCard : Config msg -> GeneralSettings -> Accordion.State -> Tooltip.States -> Posix -> Accordion.Card msg
generalSettingsCard config settings accordionState tooltipStates generatedOn =
    let
        cardId =
            "generalSettingsCard"
    in
    Accordion.card
        { id = cardId
        , options = [ markOpenedAccordionCard cardId accordionState ]
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ Html.text "Obecná nastavení" ]
        , blocks =
            [ Accordion.block []
                [ TargetPortfolioSize.form config.targetPortfolioSizeConfig settings.targetPortfolioSize
                , ReservationSetting.form config.reservationSettingChanged settings.reservationSetting
                , ExitConfig.form config.exitConfigConfig settings.exitConfig generatedOn tooltipStates
                ]
            ]
        }
