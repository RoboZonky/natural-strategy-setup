module View.Strategy exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Strategy exposing (..)
import Html exposing (Html, button, caption, div, h1, h2, input, label, option, select, table, td, text, textarea, th, tr, ul)
import Types exposing (..)
import View.Confirmation as Confirmation
import View.Filter.FilterCreationModal as FilterCreationModal
import View.FilterList as FilterList
import View.Investment as Investment
import View.InvestmentShare as InvestmentShare
import View.PortfolioStructure as PortfolioStructure
import View.TargetBalance as TargetBalance
import View.TargetPortfolioSize as TargetPortfolioSize


form : StrategyConfiguration -> Accordion.State -> Modal.State -> Grid.Column Msg
form config accordionState modalState =
    Grid.col
        [ Col.xs6 ]
        [ strategyForm config accordionState
        , FilterCreationModal.view modalState
        ]


strategyForm : StrategyConfiguration -> Accordion.State -> Html Msg
strategyForm { generalSettings, portfolioShares, investmentSizeOverrides, buyFilters, sellFilters } accordionState =
    Accordion.config AccordionMsg
        |> Accordion.cards
            [ generalSettingsCard generalSettings
            , PortfolioStructure.form generalSettings.portfolio portfolioShares
            , Investment.form generalSettings.defaultInvestmentSize investmentSizeOverrides
            , FilterList.form buyFilters
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
