module View.BuyingConfig exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Radio as Radio
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter as Filter exposing (BuyConf, BuyingConfiguration, FilteredItem(..), MarketplaceEnablement)
import Data.Tooltip as Tooltip
import DomId exposing (DomId)
import Html exposing (Html, div, text)
import Types exposing (CreationModalMsg(ModalStateMsg), Msg(CreationModalMsg, RemoveBuyFilter, SetBuyingConfiguration, TogglePrimaryMarket, ToggleSecondaryMarket))
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
            [ Accordion.block [] [ buyingConfigurationRadios buyingConfiguration ] ]
        }


buyingConfigurationRadios : BuyingConfiguration -> CardBlock.Item Msg
buyingConfigurationRadios buyingConfiguration =
    CardBlock.custom <|
        div []
            [ buyingConfigurationRadio buyingConfiguration Filter.InvEverything
            , buyingConfigurationRadio buyingConfiguration Filter.InvSomething
            , viewBuyingConfiguration buyingConfiguration
            , buyingConfigurationRadio buyingConfiguration Filter.InvNothing
            ]


viewBuyingConfiguration : BuyingConfiguration -> Html Msg
viewBuyingConfiguration buyingConfiguration =
    case buyingConfiguration of
        Filter.InvestSomething enablement filters ->
            div [ Spacing.px4 ]
                [ primarySecondaryEnablementCheckboxes enablement
                , filterCreationButtons enablement
                , filterListView RemoveBuyFilter filters
                ]

        _ ->
            text ""


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


buyingConfigurationRadio : BuyingConfiguration -> BuyConf -> Html Msg
buyingConfigurationRadio currentConfiguration thisRadiosConf =
    Radio.radio
        [ Radio.id (toString thisRadiosConf)
        , Radio.name "buyingConfiguration"
        , Radio.checked <| Filter.toBuyConfEnum currentConfiguration == thisRadiosConf
        , Radio.onClick (SetBuyingConfiguration thisRadiosConf)
        ]
        (Filter.buyConfRadioLabel thisRadiosConf)


filterCreationButtons : MarketplaceEnablement -> Html Msg
filterCreationButtons { primaryEnabled, secondaryEnabled } =
    let
        lbl =
            text "Přidat pravidlo pro "

        primButton =
            filterCreationButton Loan "Primární trh"

        secButton =
            filterCreationButton Participation "Sekundární trh"
    in
    case ( primaryEnabled, secondaryEnabled ) of
        ( True, True ) ->
            div [] [ lbl, primButton, secButton, filterCreationButton Loan_And_Participation "Oba trhy" ]

        ( True, False ) ->
            div [] [ lbl, primButton ]

        ( False, True ) ->
            div [] [ lbl, secButton ]

        ( False, False ) ->
            text ""


filterCreationButton : FilteredItem -> String -> Html Msg
filterCreationButton filteredItem buttonText =
    Button.button
        [ Button.primary
        , Button.small
        , Button.onClick <| CreationModalMsg <| ModalStateMsg filteredItem Modal.shown
        , Button.attrs [ Spacing.mx1 ]
        ]
        [ text buttonText ]
