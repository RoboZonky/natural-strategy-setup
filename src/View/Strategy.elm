module View.Strategy exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration)
import Data.Tooltip as Tooltip
import Html exposing (Html, text)
import Time.Date exposing (Date)
import Types exposing (Msg(AccordionMsg, CreationModalMsg, DeletionModalMsg))
import View.BuyingConfig as BuyingConfig
import View.CardHeightWorkaround exposing (markOpenedAccordionCard)
import View.Confirmation as Confirmation
import View.ExitConfig as ExitConfig
import View.Filter.CreationModal as FilterCreationModal
import View.Filter.DeletionModal as FilterDeletionModal
import View.Investment as Investment
import View.InvestmentShare as InvestmentShare
import View.PortfolioStructure as PortfolioStructure
import View.SellConfig as SellConfig
import View.TargetBalance as TargetBalance
import View.TargetPortfolioSize as TargetPortfolioSize


form : StrategyConfiguration -> Accordion.State -> FilterCreationModal.Model -> FilterDeletionModal.Model -> Tooltip.States -> Date -> Grid.Column Msg
form config accordionState filterCreationState filterDeletionState tooltipStates generatedOn =
    Grid.col
        [ Col.xs6 ]
        [ strategyForm config accordionState tooltipStates generatedOn
        , Html.map CreationModalMsg <| FilterCreationModal.view filterCreationState tooltipStates
        , Html.map DeletionModalMsg <| FilterDeletionModal.view filterDeletionState
        ]


strategyForm : StrategyConfiguration -> Accordion.State -> Tooltip.States -> Date -> Html Msg
strategyForm { generalSettings, portfolioShares, investmentSizeOverrides, buyingConfig, sellingConfig } accordionState tooltipStates generatedOn =
    Accordion.config AccordionMsg
        |> Accordion.onlyOneOpen
        |> Accordion.cards
            [ generalSettingsCard generalSettings accordionState tooltipStates generatedOn
            , PortfolioStructure.form generalSettings.portfolio portfolioShares accordionState tooltipStates
            , Investment.form generalSettings.defaultInvestmentSize investmentSizeOverrides
            , BuyingConfig.form buyingConfig accordionState tooltipStates
            , SellConfig.form sellingConfig accordionState tooltipStates
            ]
        |> Accordion.view accordionState


generalSettingsCard : GeneralSettings -> Accordion.State -> Tooltip.States -> Date -> Accordion.Card Msg
generalSettingsCard settings accordionState tooltipStates generatedOn =
    let
        cardId =
            "generalSettigsCard"
    in
    Accordion.card
        { id = cardId
        , options = [ markOpenedAccordionCard cardId accordionState ]
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text "Obecná nastavení" ]
        , blocks =
            [ Accordion.block []
                [ TargetPortfolioSize.form settings.targetPortfolioSize
                , InvestmentShare.form settings.defaultInvestmentShare
                , TargetBalance.form settings.defaultTargetBalance
                , Confirmation.form settings.confirmationSettings tooltipStates
                , ExitConfig.form settings.exitConfig generatedOn tooltipStates
                ]
            ]
        }
