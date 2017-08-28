module View.FilterList exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Data.Filter exposing (MarketplaceFilter, renderMarketplaceFilter)
import Html exposing (Html, button, div, h2, h3, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (Msg(ModalMsg, RemoveBuyFilter))


form : List MarketplaceFilter -> Accordion.Card Msg
form filters =
    Accordion.card
        { id = "buyFiltersCard"
        , options = []
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text "Filtrování tržiště" ]
        , blocks =
            [ Accordion.block [] [ filtersView filters, filterCreationControls ] ]
        }


filtersView : List MarketplaceFilter -> Card.BlockItem Msg
filtersView filters =
    Card.custom <| ListGroup.ul <| List.indexedMap viewFilter filters


viewFilter : Int -> MarketplaceFilter -> ListGroup.Item Msg
viewFilter index mf =
    let
        removeButton =
            Button.button
                [ Button.danger
                , Button.small
                , Button.onClick (RemoveBuyFilter index)
                , Button.attrs [ class "float-right" ]
                ]
                [ text "Odebrat" ]
    in
    ListGroup.li []
        [ text <| renderMarketplaceFilter mf ++ " "
        , removeButton
        ]


filterCreationControls : Card.BlockItem Msg
filterCreationControls =
    Card.custom <|
        Button.button
            [ Button.primary
            , Button.attrs [ onClick <| ModalMsg Modal.visibleState ]
            ]
            [ text "Přidat Filtr" ]
