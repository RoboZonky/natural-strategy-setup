module View.Strategy exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration)
import Data.Tooltip as Tooltip
import Html exposing (Html, text)
import Types exposing (Msg(AccordionMsg, ModalMsg))
import View.BuyingConfig as BuyingConfig
import View.Confirmation as Confirmation
import View.Filter.FilterCreationModal as FilterCreationModal
import View.Investment as Investment
import View.InvestmentShare as InvestmentShare
import View.PortfolioStructure as PortfolioStructure
import View.SellFilterList as SellFilterList
import View.TargetBalance as TargetBalance
import View.TargetPortfolioSize as TargetPortfolioSize


form : StrategyConfiguration -> Accordion.State -> FilterCreationModal.Model -> Tooltip.States -> Grid.Column Msg
form config accordionState filterCreationState tooltipStates =
    Grid.col
        [ Col.xs6 ]
        [ strategyForm config accordionState tooltipStates
        , Html.map ModalMsg <| FilterCreationModal.view filterCreationState tooltipStates
        ]


strategyForm : StrategyConfiguration -> Accordion.State -> Tooltip.States -> Html Msg
strategyForm { generalSettings, portfolioShares, investmentSizeOverrides, buyingConfig, sellFilters } accordionState tooltipStates =
    Accordion.config AccordionMsg
        |> Accordion.cards
            [ generalSettingsCard generalSettings
            , PortfolioStructure.form generalSettings.portfolio portfolioShares tooltipStates
            , Investment.form generalSettings.defaultInvestmentSize investmentSizeOverrides
            , BuyingConfig.form buyingConfig tooltipStates
            , SellFilterList.form sellFilters tooltipStates
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
