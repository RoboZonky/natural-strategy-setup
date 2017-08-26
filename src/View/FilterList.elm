module View.FilterList exposing (form)

import Data.Filter exposing (MarketplaceFilter, renderMarketplaceFilter)
import Html exposing (Html, button, div, h2, h3, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Types exposing (Msg(RemoveBuyFilter))


form : List MarketplaceFilter -> Html Msg
form list =
    div [] <|
        [ h2 [] [ text "Filtrování tržiště" ] ]
            ++ List.indexedMap viewFilter list
            ++ [ filterCreationControls ]


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


filterCreationControls : Html Msg
filterCreationControls =
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
