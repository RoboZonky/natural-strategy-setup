module View.Strategy exposing (..)

import Data.BuyFilter exposing (BuyFilter)
import Data.PortfolioShare as PortfolioShare exposing (PortfolioShare, PortfolioShares)
import Data.SellFilter exposing (SellFilter)
import Data.Strategy exposing (..)
import Html exposing (Html, button, caption, div, h2, input, label, option, select, table, td, text, textarea, th, tr, ul)
import Types exposing (..)
import View.Confirmation as Confirmation
import View.Investment as Investment
import View.InvestmentShare as InvestmentShare
import View.PortfolioStructure as PortfolioStructure
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
        , PortfolioStructure.portfolioSharesForm generalSettings.portfolio portfolioShares
        , Investment.investmentForm generalSettings.defaultInvestmentSize investmentSizeOverrides
        , buyFiltersForm buyFilters
        , sellFiltersForm sellFilters
        ]


generalSettingsForm : GeneralSettings -> Html Msg
generalSettingsForm generalSettings =
    div []
        [ h2 [] [ text "Obecná nastavení" ]
        , TargetPortfolioSize.form generalSettings.targetPortfolioSize
        , InvestmentShare.form generalSettings.defaultInvestmentShare
        , Confirmation.form generalSettings.confirmationSettings
        ]


portfolioView : PortfolioShare -> Html Msg
portfolioView portfolioShare =
    Html.li []
        [ text <| PortfolioShare.renderPortfolioShare portfolioShare
        , button [] [ text "Odebrat" ]
        ]


buyFiltersForm : List BuyFilter -> Html Msg
buyFiltersForm filters =
    div []
        [ h2 [] [ text "Filtrování tržiště" ]
        , button [] [ text "Přidat filtr" ]
        ]


sellFiltersForm : List SellFilter -> Html Msg
sellFiltersForm filters =
    div []
        [ h2 [] [ text "Prodej participací" ]
        , button [] [ text "Přidat filtr" ]
        ]


listView : (a -> Html Msg) -> List a -> Html Msg
listView itemView list =
    case list of
        [] ->
            text "Bez úprav"

        nonempty ->
            ul [] <| List.map itemView nonempty
