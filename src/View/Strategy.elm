module View.Strategy exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration)
import Data.Tooltip as Tooltip
import Html exposing (Html, text)
import Types exposing (Msg(AccordionMsg, CreationModalMsg, DeletionModalMsg))
import View.BuyingConfig as BuyingConfig
import View.Confirmation as Confirmation
import View.Filter.FilterCreationModal as FilterCreationModal
import View.Filter.FilterDeletionModal as FilterDeletionModal
import View.Investment as Investment
import View.InvestmentShare as InvestmentShare
import View.PortfolioStructure as PortfolioStructure
import View.SellConfig as SellConfig
import View.TargetBalance as TargetBalance
import View.TargetPortfolioSize as TargetPortfolioSize


form : StrategyConfiguration -> Accordion.State -> FilterCreationModal.Model -> FilterDeletionModal.Model -> Tooltip.States -> Grid.Column Msg
form config accordionState filterCreationState filterDeletionState tooltipStates =
    Grid.col
        [ Col.xs6 ]
        [ strategyForm config accordionState tooltipStates
        , Html.map CreationModalMsg <| FilterCreationModal.view filterCreationState tooltipStates
        , Html.map DeletionModalMsg <| FilterDeletionModal.view filterDeletionState
        ]


strategyForm : StrategyConfiguration -> Accordion.State -> Tooltip.States -> Html Msg
strategyForm { generalSettings, portfolioShares, investmentSizeOverrides, buyingConfig, sellingConfig } accordionState tooltipStates =
    Accordion.config AccordionMsg
        |> Accordion.cards
            [ generalSettingsCard generalSettings
            , PortfolioStructure.form generalSettings.portfolio portfolioShares tooltipStates
            , Investment.form generalSettings.defaultInvestmentSize investmentSizeOverrides
            , BuyingConfig.form buyingConfig tooltipStates
            , SellConfig.form sellingConfig tooltipStates
            ]
        |> Accordion.view accordionState


generalSettingsCard : GeneralSettings -> Accordion.Card Msg
generalSettingsCard { targetPortfolioSize, defaultInvestmentShare, defaultTargetBalance, confirmationSettings } =
    Accordion.card
        { id = "generalSettigsCard"
        , options = []
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text "Obecná nastavení" ]
        , blocks =
            [ Accordion.block []
                [ TargetPortfolioSize.form targetPortfolioSize
                , InvestmentShare.form defaultInvestmentShare
                , TargetBalance.form defaultTargetBalance
                , Confirmation.form confirmationSettings
                ]
            ]
        }
