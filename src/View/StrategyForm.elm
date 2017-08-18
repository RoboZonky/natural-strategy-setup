module View.StrategyForm exposing (..)

import Data.BuyFilter exposing (BuyFilter)
import Data.Investment as Investment exposing (InvestmentPerRating)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioShare as PortfolioShare exposing (PortfolioShare, PortfolioShares)
import Data.SellFilter exposing (SellFilter)
import Data.Strategy exposing (..)
import Html exposing (Html, button, caption, div, h2, input, label, option, select, table, td, text, textarea, th, tr, ul)
import Html.Attributes as Attr exposing (checked, cols, disabled, height, name, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)
import View.TargetPortfolioSize
import EveryDict as Dict
import Data.Rating exposing (ratingToString)


view : Model -> Html Msg
view model =
    let
        ( isSimple, subform ) =
            case model of
                SimpleStrategy defaultPortfolio ->
                    ( True, defaultPortfolioForm )

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


defaultPortfolioForm : Html Msg
defaultPortfolioForm =
    div [] [ text "Robot má udržovat ", defaultPortfolioSelect, text " portfolio." ]


complexStrategyFormView : ComplexStrategyParameters -> Html Msg
complexStrategyFormView params =
    div []
        [ generalSettingsForm params.generalSettings
        , portfolioSharesForm params.portfolioShares
        , investmentPerRatingForm params.investmentPerRating
        , buyFiltersForm params.buyFilters
        , sellFiltersForm params.sellFilters
        ]


generalSettingsForm : GeneralSettings -> Html Msg
generalSettingsForm generalSettings =
    div []
        [ h2 [] [ text "Obecná nastavení" ]
        , View.TargetPortfolioSize.form generalSettings.targetPortfolioSize
        ]


portfolioSharesForm : PortfolioShares -> Html Msg
portfolioSharesForm shares =
    div []
        [ h2 [] [ text "Úprava struktury portfolia" ]
        , defaultPortfolioForm
        , ratingSharesTable shares
        ]


ratingSharesTable : PortfolioShares -> Html Msg
ratingSharesTable shares =
    let
        rows =
            List.map portfolioShareRow <| Dict.toList shares
    in
        table [] <|
            [ caption []
                [ text "Požadovaný podíl půjček v daném ratingu (v %)" ]
            , tr []
                [ th [] [ text "Rating" ]
                , th [] [ text "min" ]
                , th [] [ text "max" ]
                ]
            ]
                ++ rows


portfolioShareRow : PortfolioShare -> Html Msg
portfolioShareRow ( rtg, ( mi, mx ) ) =
    tr []
        [ td [] [ text <| ratingToString rtg ]
        , td [] [ text <| toString mi ]
        , td [] [ text <| toString mx ]
        ]


portfolioView : PortfolioShare -> Html Msg
portfolioView portfolioShare =
    Html.li []
        [ text <| PortfolioShare.renderPortfolioShare portfolioShare
        , button [] [ text "Odebrat" ]
        ]


investmentPerRatingForm : List InvestmentPerRating -> Html Msg
investmentPerRatingForm iprs =
    div []
        [ h2 [] [ text "Výše investice" ]
        , listView investmentPerRatingView iprs
        , button [] [ text "Přidat omezení" ]
        ]


investmentPerRatingView : InvestmentPerRating -> Html Msg
investmentPerRatingView ipr =
    Html.li []
        [ text <| Investment.renderInvestment ipr
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


defaultPortfolioSelect : Html Msg
defaultPortfolioSelect =
    select [ onInput (PortfolioChanged << Portfolio.fromString) ] <|
        List.map
            (\portfolio ->
                option
                    [ value (toString portfolio) ]
                    [ text (Portfolio.toString portfolio) ]
            )
            [ Conservative, Balanced, Progressive, Empty ]
