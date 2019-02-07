module View.BuyingConfig exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter as Filter exposing (BuyingConfiguration, FilteredItem(..), MarketplaceEnablement)
import Data.Filter.Complexity exposing (FilterComplexity(..), complexityButtonLabel)
import Data.Tooltip as Tooltip
import DomId exposing (DomId)
import Html exposing (Html, div, text)
import Types exposing (CreationModalMsg(..), Msg(..))
import Version exposing (filtersHowToLink)
import View.CardHeightWorkaround exposing (markOpenedAccordionCard)
import View.Filter exposing (filterListView)
import View.Tooltip as Tooltip


form : BuyingConfiguration -> Accordion.State -> Tooltip.States -> Accordion.Card Msg
form buyingConfiguration accordionState tooltipStates =
    let
        cardId =
            "buyingConfigCard"
    in
    Accordion.card
        { id = cardId
        , options = [ markOpenedAccordionCard cardId accordionState ]
        , header =
            Accordion.headerH4 [] (Accordion.toggle [] [ text "Pravidla nákupu" ])
                |> Accordion.appendHeader [ Tooltip.popoverTip Tooltip.buyFilterListTip tooltipStates ]
        , blocks =
            [ Accordion.block [] [ viewBuyingConfiguration buyingConfiguration ] ]
        }


viewBuyingConfiguration : BuyingConfiguration -> CardBlock.Item Msg
viewBuyingConfiguration buyingConfiguration =
    let
        enablement =
            Filter.getMarketplaceEnablement buyingConfiguration

        additionalControls =
            case buyingConfiguration of
                Filter.InvestSomething _ filters ->
                    [ filterListView RemoveBuyFilter filters
                    , filterCreationButtons enablement
                    ]

                Filter.InvestEverything ->
                    [ filterCreationButtons enablement ]

                Filter.InvestNothing ->
                    []
    in
    CardBlock.custom <|
        div [] <|
            primarySecondaryEnablementCheckboxes enablement
                :: additionalControls


primarySecondaryEnablementCheckboxes : MarketplaceEnablement -> Html Msg
primarySecondaryEnablementCheckboxes enablement =
    div []
        [ marketplaceEnablementCheckbox TogglePrimaryMarket (not enablement.primaryEnabled) "me1" "Ignorovat všechny půjčky."
        , marketplaceEnablementCheckbox ToggleSecondaryMarket (not enablement.secondaryEnabled) "me2" "Ignorovat všechny participace."
        ]


marketplaceEnablementCheckbox : (Bool -> Msg) -> Bool -> DomId -> String -> Html Msg
marketplaceEnablementCheckbox tag isChecked domId label =
    Checkbox.checkbox
        [ Checkbox.id domId
        , Checkbox.onCheck (tag << not)
        , Checkbox.checked isChecked
        ]
        label


filterCreationButtons : MarketplaceEnablement -> Html Msg
filterCreationButtons marketplaceEnablement =
    let
        allowedFilteredItems =
            Filter.getAllowedFilterItems marketplaceEnablement
    in
    div []
        [ filterCreationButton allowedFilteredItems Simple Button.primary
        , filterCreationButton allowedFilteredItems Complex Button.outlineSecondary
        , filtersHowToLink
        ]


filterCreationButton : List FilteredItem -> FilterComplexity -> Button.Option Msg -> Html Msg
filterCreationButton allowedFilteredItems filterComplexity buttonType =
    Button.button
        [ buttonType
        , Button.small
        , Button.onClick <| CreationModalMsg <| OpenCreationModal filterComplexity allowedFilteredItems
        , Button.attrs [ Spacing.mx1 ]
        ]
        [ text <| complexityButtonLabel filterComplexity ]
