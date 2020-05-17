module View.BuyingConfig exposing (Config, form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter as Filter exposing (BuyingConfiguration, FilteredItem(..), MarketplaceEnablement)
import Data.Filter.Complexity exposing (FilterComplexity(..), complexityButtonLabel)
import Data.Tooltip as Tooltip
import DomId exposing (DomId)
import Html exposing (Html)
import Version exposing (filtersHowToLink)
import View.CardHeightWorkaround exposing (markOpenedAccordionCard)
import View.Filter exposing (filterListView)
import View.Tooltip as Tooltip


type alias Config msg =
    { tooltipConfig : Tooltip.Config msg
    , removeBuyFilter : Int -> msg
    , togglePrimaryMarket : Bool -> msg
    , toggleSecondaryMarket : Bool -> msg
    , openCreationModal : FilterComplexity -> List FilteredItem -> msg
    }


form : Config msg -> BuyingConfiguration -> Accordion.State -> Tooltip.States -> Accordion.Card msg
form config buyingConfiguration accordionState tooltipStates =
    let
        cardId =
            "buyingConfigCard"
    in
    Accordion.card
        { id = cardId
        , options = [ markOpenedAccordionCard cardId accordionState ]
        , header =
            Accordion.headerH4 [] (Accordion.toggle [] [ Html.text "Pravidla nákupu" ])
                |> Accordion.appendHeader [ Tooltip.popoverTip config.tooltipConfig Tooltip.buyFilterListTip tooltipStates ]
        , blocks =
            [ Accordion.block [] [ viewBuyingConfiguration config buyingConfiguration ] ]
        }


viewBuyingConfiguration : Config msg -> BuyingConfiguration -> CardBlock.Item msg
viewBuyingConfiguration config buyingConfiguration =
    let
        enablement =
            Filter.getMarketplaceEnablement buyingConfiguration

        additionalControls =
            case buyingConfiguration of
                Filter.InvestSomething _ filters ->
                    [ filterListView config.removeBuyFilter filters
                    , filterCreationButtons config enablement
                    ]

                Filter.InvestEverything ->
                    [ filterCreationButtons config enablement ]

                Filter.InvestNothing ->
                    []
    in
    CardBlock.custom <|
        Html.div [] <|
            primarySecondaryEnablementCheckboxes config enablement
                :: additionalControls


primarySecondaryEnablementCheckboxes : Config msg -> MarketplaceEnablement -> Html msg
primarySecondaryEnablementCheckboxes config enablement =
    Html.div []
        [ marketplaceEnablementCheckbox config.togglePrimaryMarket (not enablement.primaryEnabled) "me1" "Ignorovat všechny půjčky."
        , marketplaceEnablementCheckbox config.toggleSecondaryMarket (not enablement.secondaryEnabled) "me2" "Ignorovat všechny participace."
        ]


marketplaceEnablementCheckbox : (Bool -> msg) -> Bool -> DomId -> String -> Html msg
marketplaceEnablementCheckbox tag isChecked domId label =
    Checkbox.checkbox
        [ Checkbox.id domId
        , Checkbox.onCheck (tag << not)
        , Checkbox.checked isChecked
        ]
        label


filterCreationButtons : Config msg -> MarketplaceEnablement -> Html msg
filterCreationButtons config marketplaceEnablement =
    let
        allowedFilteredItems =
            Filter.getAllowedFilterItems marketplaceEnablement
    in
    Html.div []
        [ filterCreationButton config allowedFilteredItems Simple Button.primary
        , filterCreationButton config allowedFilteredItems Complex Button.outlineSecondary
        , filtersHowToLink
        ]


filterCreationButton : Config msg -> List FilteredItem -> FilterComplexity -> Button.Option msg -> Html msg
filterCreationButton config allowedFilteredItems filterComplexity buttonType =
    Button.button
        [ buttonType
        , Button.small
        , Button.onClick <| config.openCreationModal filterComplexity allowedFilteredItems
        , Button.attrs [ Spacing.mx1 ]
        ]
        [ Html.text <| complexityButtonLabel filterComplexity ]
