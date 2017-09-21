module View.FilterList exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Filter exposing (FilteredItem(..), MarketplaceFilter, renderMarketplaceFilter)
import Data.Tooltip as Tooltip
import Html exposing (Html, button, div, h2, h3, pre, span, text)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Types exposing (ModalMsg(ModalStateMsg), Msg(ModalMsg, RemoveBuyFilter))
import View.Tooltip as Tooltip


form : List MarketplaceFilter -> Tooltip.States -> Accordion.Card Msg
form filters tooltipStates =
    Accordion.card
        { id = "buyFiltersCard"
        , options = []
        , header =
            Accordion.headerH4 [] <|
                Accordion.toggle []
                    [ text "Filtrování tržiště"
                    , Tooltip.popoverTip Tooltip.filterListTip tooltipStates
                    ]
        , blocks =
            [ Accordion.block [] [ filtersView filters, filterCreationControls ] ]
        }


filtersView : List MarketplaceFilter -> Card.BlockItem Msg
filtersView filters =
    Card.custom <| div [] <| List.indexedMap viewFilter filters


viewFilter : Int -> MarketplaceFilter -> Html Msg
viewFilter index mf =
    let
        removeButton =
            span [ onClick (RemoveBuyFilter index), class "float-right" ] [ text "✖" ]

        filterText =
            span [] [ text <| renderMarketplaceFilter mf ]
    in
    Card.config []
        |> Card.block [ Card.blockAttrs [ class "smaller-pad" ] ]
            [ Card.custom <|
                Grid.row []
                    [ Grid.col [ Col.xs11 ] [ filterText ]
                    , Grid.col [ Col.xs1 ] [ removeButton ]
                    ]
            ]
        |> Card.view


filterCreationControls : Card.BlockItem Msg
filterCreationControls =
    let
        creationButton filteredItem buttonText =
            Button.button
                [ Button.primary
                , Button.onClick <| ModalMsg <| ModalStateMsg filteredItem Modal.visibleState
                , Button.attrs [ class "mx-1" ]
                ]
                [ text buttonText ]
    in
    Card.custom <|
        div []
            [ text "Přidat filtr pro "
            , creationButton Loan "Úvěry"
            , creationButton Participation "Participace"
            , creationButton Loan_And_Participation "Obojí"
            ]
