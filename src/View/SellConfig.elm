module View.SellConfig exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter exposing (FilteredItem(..), SellingConfiguration(..))
import Data.Filter.Complexity exposing (FilterComplexity(..), complexityButtonLabel)
import Data.Tooltip as Tooltip
import Html exposing (Html, div, text)
import Types exposing (CreationModalMsg(..), Msg(..))
import Version exposing (filtersHowToLink)
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
            , filterCreationButtons
            ]


filterCreationButtons : Html Msg
filterCreationButtons =
    div []
        [ filterCreationButton Simple Button.primary
        , filterCreationButton Complex Button.outlineSecondary
        , filtersHowToLink
        ]


filterCreationButton : FilterComplexity -> Button.Option Msg -> Html Msg
filterCreationButton filterComplexity buttonType =
    Button.button
        [ buttonType
        , Button.onClick <| CreationModalMsg <| OpenCreationModal filterComplexity [ Participation_To_Sell ]
        , Button.attrs [ Spacing.mx1 ]
        , Button.small
        ]
        [ text <| complexityButtonLabel filterComplexity ]
