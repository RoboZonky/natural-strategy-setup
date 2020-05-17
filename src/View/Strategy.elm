module View.Strategy exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Filter as Filter
import Data.Investment as Investment
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration)
import Data.Tooltip as Tooltip
import Html exposing (Html)
import Time exposing (Posix)
import Types exposing (Msg(..))
import View.BuyingConfig as BuyingConfig
import View.CardHeightWorkaround exposing (markOpenedAccordionCard)
import View.ExitConfig as ExitConfig
import View.Filter.CreationModal as FilterCreationModal
import View.Filter.DeletionModal as FilterDeletionModal
import View.PortfolioStructure as PortfolioStructure
import View.ReservationSetting as ReservationSetting
import View.SellConfig as SellConfig
import View.TargetPortfolioSize as TargetPortfolioSize


form : StrategyConfiguration -> Accordion.State -> FilterCreationModal.Model -> FilterDeletionModal.Model -> Tooltip.States -> Posix -> Grid.Column Msg
form config accordionState filterCreationState filterDeletionState tooltipStates generatedOn =
    Grid.col
        [ Col.xs6 ]
        [ strategyForm config accordionState tooltipStates generatedOn
        , Html.map CreationModalMsg <| FilterCreationModal.view filterCreationState tooltipStates
        , FilterDeletionModal.view deletionModalConfig filterDeletionState
        ]


deletionModalConfig : FilterDeletionModal.Config Msg
deletionModalConfig =
    { setBuyingConfig = SetBuyingConfig
    , setSellingConfig = SetSellingConfig
    }


strategyForm : StrategyConfiguration -> Accordion.State -> Tooltip.States -> Posix -> Html Msg
strategyForm { generalSettings, portfolioStructure, primaryInvestmentOverrides, secondaryPurchaseOverrides, buyingConfig, sellingConfig } accordionState tooltipStates generatedOn =
    Accordion.config AccordionMsg
        |> Accordion.onlyOneOpen
        |> Accordion.cards
            [ generalSettingsCard generalSettings accordionState tooltipStates generatedOn
            , PortfolioStructure.form generalSettings.portfolio portfolioStructure accordionState tooltipStates
            , Investment.form
                primaryInvestmentConfig
                (Filter.isBuyingOnPrimaryEnabled buyingConfig)
                generalSettings.defaultPrimaryInvestmentSize
                primaryInvestmentOverrides
            , Investment.form secondaryPurchaseConfig
                (Filter.isBuyingOnSecondaryEnabled buyingConfig)
                generalSettings.defaultSecondaryPurchaseSize
                secondaryPurchaseOverrides
            , BuyingConfig.form buyingConfig accordionState tooltipStates
            , SellConfig.form sellingConfig accordionState tooltipStates
            ]
        |> Accordion.view accordionState


primaryInvestmentConfig : Investment.Config Msg
primaryInvestmentConfig =
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
    , onDefaultInvestmentChange = DefaultPrimaryInvestmentChanged
    , onInvestmentChange = PrimaryInvestmentChanged
    , noOp = NoOp
    }


secondaryPurchaseConfig : Investment.Config Msg
secondaryPurchaseConfig =
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
    , onDefaultInvestmentChange = DefaultSecondaryPurchaseChanged
    , onInvestmentChange = SecondaryPurchaseChanged
    , noOp = NoOp
    }


generalSettingsCard : GeneralSettings -> Accordion.State -> Tooltip.States -> Posix -> Accordion.Card Msg
generalSettingsCard settings accordionState tooltipStates generatedOn =
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
                [ TargetPortfolioSize.form settings.targetPortfolioSize
                , ReservationSetting.form settings.reservationSetting
                , ExitConfig.form settings.exitConfig generatedOn tooltipStates
                ]
            ]
        }
