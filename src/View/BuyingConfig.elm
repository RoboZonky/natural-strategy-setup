module View.BuyingConfig exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (BuyConf, BuyingConfiguration, FilteredItem(..), MarketplaceEnablement, MarketplaceFilter, renderBuyFilter)
import Data.Tooltip as Tooltip
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (CreationModalMsg(ModalStateMsg), Msg(CreationModalMsg, RemoveBuyFilter, SetBuyingConfiguration, TogglePrimaryMarket, ToggleSecondaryMarket))
import View.Tooltip as Tooltip


form : BuyingConfiguration -> Tooltip.States -> Accordion.Card Msg
form buyingConfiguration tooltipStates =
    Accordion.card
        { id = "buyingConfigCard"
        , options = []
        , header =
            Accordion.headerH4 [] (Accordion.toggle [] [ text "Pravidla pro nákup" ])
                |> Accordion.appendHeader [ Tooltip.popoverTip Tooltip.buyFilterListTip tooltipStates ]
        , blocks =
            [ Accordion.block [] [ buyingConfigurationRadios buyingConfiguration ] ]
        }


buyingConfigurationRadios : BuyingConfiguration -> Card.BlockItem Msg
buyingConfigurationRadios buyingConfiguration =
    Card.custom <|
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
            div [ class "px-4" ]
                [ primarySecondaryEnablementCheckboxes enablement
                , filterCreationButtons enablement
                , filtersView filters
                ]

        _ ->
            text ""


primarySecondaryEnablementCheckboxes : MarketplaceEnablement -> Html Msg
primarySecondaryEnablementCheckboxes enablement =
    let
        checkbox tag isChecked =
            Checkbox.checkbox
                [ Checkbox.onCheck (tag << not)
                , Checkbox.checked isChecked
                ]
    in
    div []
        [ checkbox TogglePrimaryMarket (not enablement.primaryEnabled) "Ignorovat všechny půjčky."
        , checkbox ToggleSecondaryMarket (not enablement.secondaryEnabled) "Ignorovat všechny participace."
        ]


buyingConfigurationRadio : BuyingConfiguration -> BuyConf -> Html Msg
buyingConfigurationRadio currentConfiguration thisRadiosConf =
    Radio.radio
        [ Radio.name "buyingConfiguration"
        , Radio.checked <| Filter.toBuyConfEnum currentConfiguration == thisRadiosConf
        , Radio.onClick (SetBuyingConfiguration thisRadiosConf)
        ]
        (Filter.buyConfRadioLabel thisRadiosConf)


filtersView : List MarketplaceFilter -> Html Msg
filtersView filters =
    div [ class "p-2" ] <| List.indexedMap viewFilter filters


viewFilter : Int -> MarketplaceFilter -> Html Msg
viewFilter index mf =
    let
        removeButton =
            span [ onClick (RemoveBuyFilter index), class "float-right" ] [ text "✖" ]

        filterText =
            span [] [ text <| renderBuyFilter mf ]
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
        , Button.onClick <| CreationModalMsg <| ModalStateMsg filteredItem Modal.visibleState
        , Button.attrs [ class "mx-1" ]
        ]
        [ text buttonText ]
