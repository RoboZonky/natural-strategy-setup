module View.SellConfig exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter exposing (FilteredItem(Participation_To_Sell), SellingConfiguration(..))
import Data.Tooltip as Tooltip
import Html exposing (Html, div, text)
import Types exposing (CreationModalMsg(ModalStateMsg), Msg(CreationModalMsg, RemoveSellFilter))
import View.CardHeightWorkaround exposing (markOpenedAccordionCard)
import View.Filter exposing (filterListView)
import View.Tooltip as Tooltip


form : SellingConfiguration -> Accordion.State -> Tooltip.States -> Accordion.Card Msg
form sellingConfiguration accordionState tooltipStates =
    let
        cardId =
            "sellingConfigCard"
    in
    Accordion.card
        { id = cardId
        , options = [ markOpenedAccordionCard cardId accordionState ]
        , header =
            Accordion.headerH4 [] (Accordion.toggle [] [ text "Pravidla prodeje" ])
                |> Accordion.appendHeader [ Tooltip.popoverTip Tooltip.sellFilterListTip tooltipStates ]
        , blocks =
            [ Accordion.block [] [ viewSellingConfiguration sellingConfiguration ] ]
        }


viewSellingConfiguration : SellingConfiguration -> CardBlock.Item Msg
viewSellingConfiguration sellingConfiguration =
    let
        emptyListMessageOrFilterList =
            case sellingConfiguration of
                SellSomething filters ->
                    filterListView RemoveSellFilter filters

                SellNothing ->
                    div [] [ text "Prodej participací je zakázán. Chcete-li jej povolit, přidejte aspoň jedno pravidlo." ]
    in
    CardBlock.custom <|
        div []
            [ emptyListMessageOrFilterList
            , filterCreationControls
            ]


filterCreationControls : Html Msg
filterCreationControls =
    Button.button
        [ Button.primary
        , Button.onClick <| CreationModalMsg <| ModalStateMsg Participation_To_Sell Modal.shown
        , Button.attrs [ Spacing.mx1 ]
        , Button.small
        ]
        [ text "Přidat pravidlo" ]
