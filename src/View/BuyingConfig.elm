module View.BuyingConfig exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (BuyConf, BuyingConfiguration, FilteredItem(..), MarketplaceFilter, renderBuyFilter)
import Data.Tooltip as Tooltip
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (ModalMsg(ModalStateMsg), Msg(ModalMsg, RemoveBuyFilter, SetBuyingConfiguration))
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
    let
        filterListOrNothing =
            case buyingConfiguration of
                Filter.InvestSomething filters ->
                    div [] [ filtersView filters, filterCreationButtons ]

                _ ->
                    text ""
    in
    Card.custom <|
        div []
            [ buyingConfigurationRadio buyingConfiguration Filter.InvEverything
            , buyingConfigurationRadio buyingConfiguration Filter.InvSomething
            , filterListOrNothing
            , buyingConfigurationRadio buyingConfiguration Filter.InvNothing
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
    div [] <| List.indexedMap viewFilter filters


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


filterCreationButtons : Html Msg
filterCreationButtons =
    div []
        [ text "Přidat pravidlo pro "
        , filterCreationButton Loan "Primární trh"
        , filterCreationButton Participation "Sekundární trh"
        , filterCreationButton Loan_And_Participation "Oba trhy"
        ]


filterCreationButton : FilteredItem -> String -> Html Msg
filterCreationButton filteredItem buttonText =
    Button.button
        [ Button.primary
        , Button.small
        , Button.onClick <| ModalMsg <| ModalStateMsg filteredItem Modal.visibleState
        , Button.attrs [ class "mx-1" ]
        ]
        [ text buttonText ]
