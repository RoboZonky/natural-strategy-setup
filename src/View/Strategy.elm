module View.Strategy exposing (..)

import Data.Strategy exposing (..)
import Html exposing (Html, button, caption, div, h2, input, label, option, select, table, td, text, textarea, th, tr, ul)
import Types exposing (..)
import View.Confirmation as Confirmation
import View.FilterList as FilterList
import View.Investment as Investment
import View.InvestmentShare as InvestmentShare
import View.PortfolioStructure as PortfolioStructure
import View.TargetBalance as TargetBalance
import View.TargetPortfolioSize as TargetPortfolioSize


form : StrategyConfiguration -> Html Msg
form config =
    div []
        [ h2 [] [ text "Konfigurace strategie" ]
        , strategyForm config
        ]


strategyForm : StrategyConfiguration -> Html Msg
strategyForm { generalSettings, portfolioShares, investmentSizeOverrides, buyFilters, sellFilters } =
    div []
        [ generalSettingsForm generalSettings
        , PortfolioStructure.form generalSettings.portfolio portfolioShares
        , Investment.form generalSettings.defaultInvestmentSize investmentSizeOverrides
        , FilterList.form buyFilters
        ]


generalSettingsForm : GeneralSettings -> Html Msg
generalSettingsForm { targetPortfolioSize, defaultInvestmentShare, defaultTargetBalance, confirmationSettings } =
    div []
        [ h2 [] [ text "Obecná nastavení" ]
        , TargetPortfolioSize.form targetPortfolioSize
        , InvestmentShare.form defaultInvestmentShare
        , TargetBalance.form defaultTargetBalance
        , Confirmation.form confirmationSettings
        ]
