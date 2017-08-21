module View.StrategyForm exposing (..)

import Data.BuyFilter exposing (BuyFilter)
import Data.PortfolioShare as PortfolioShare exposing (PortfolioShare, PortfolioShares)
import Data.SellFilter exposing (SellFilter)
import Data.Strategy exposing (..)
import Html exposing (Html, button, caption, div, h2, input, label, option, select, table, td, text, textarea, th, tr, ul)
import Html.Attributes as Attr exposing (checked, cols, disabled, height, min, name, rows, size, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)
import View.Confirmation as Confirmation
import View.InvestmentForm as InvestmentForm
import View.PortfolioStructure as PortfolioStructure
import View.TargetPortfolioSize


view : ParsedStrategy -> Html Msg
view model =
    let
        ( isSimple, subform ) =
            case model of
                SimpleStrategy defaultPortfolio ->
                    ( True, PortfolioStructure.defaultPortfolioForm )

                ComplexStrategy params ->
                    ( False, complexStrategyFormView params )
    in
    div []
        [ h2 [] [ text "Konfigurace strategie" ]
        , label []
            [ input [ type_ "radio", name "strategyRadio", onClick SimpleStrategySelected, checked isSimple ] []
            , text "Jednoduchá"
            ]
        , label []
            [ input [ type_ "radio", name "strategyRadio", onClick ComplexStrategySelected, checked (not isSimple) ] []
            , text "Pokročilá"
            ]
        , subform
        ]


complexStrategyFormView : ComplexStrategyParameters -> Html Msg
complexStrategyFormView params =
    div []
        [ generalSettingsForm params.generalSettings
        , PortfolioStructure.portfolioSharesForm params.generalSettings.portfolio params.portfolioShares
        , InvestmentForm.investmentForm params.investmentPerRating
        , buyFiltersForm params.buyFilters
        , sellFiltersForm params.sellFilters
        ]


generalSettingsForm : GeneralSettings -> Html Msg
generalSettingsForm generalSettings =
    div []
        [ h2 [] [ text "Obecná nastavení" ]
        , View.TargetPortfolioSize.form generalSettings.targetPortfolioSize
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
