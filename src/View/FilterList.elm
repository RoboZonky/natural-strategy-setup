module View.FilterList exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Data.Filter exposing (MarketplaceFilter, renderMarketplaceFilter)
import Html exposing (Html, button, div, h2, h3, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Types exposing (Msg(RemoveBuyFilter))


form : List MarketplaceFilter -> Accordion.Card Msg
form filters =
    Accordion.card
        { id = "buyFiltersCard"
        , options = []
        , header = Accordion.headerH3 [] <| Accordion.toggle [] [ text "Filtrování tržiště" ]
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
            button [ onClick (RemoveBuyFilter index) ] [ text "Odebrat filtr" ]
    in
    div []
        [ text <| renderMarketplaceFilter mf ++ " "
        , removeButton
        ]


filterCreationControls : Card.BlockItem Msg
filterCreationControls =
    Card.custom <|
        div []
            [ button [] [ text "Přidat filtr (TODO otevře modal)" ]
            , filterCreationModal
            ]


filterCreationModal : Html Msg
filterCreationModal =
    div [ style [ ( "border", "1px solid black" ), ( "width", "50%" ) ] ]
        [ h3 []
            [ text "Vytvořit filtr (TODO obsah modalu)" ]
        , div []
            [ text "Modal body" ]
        , div
            [{--modal footer--}
            ]
            [ button [] [ text "Přidat" ]
            , button [] [ text "zrušit" ]
            ]
        ]
