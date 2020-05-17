module View.SellConfig exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter exposing (FilteredItem(..), SellingConfiguration(..), validateSellingConfiguration)
import Data.Filter.Complexity exposing (FilterComplexity(..), complexityButtonLabel)
import Data.Tooltip as Tooltip
import Html exposing (Html)
import Types exposing (CreationModalMsg(..), Msg(..))
import Util
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
            Accordion.headerH4 [] (Accordion.toggle [] [ Html.text "Pravidla prodeje" ])
                |> Accordion.appendHeader [ Tooltip.popoverTip Tooltip.sellFilterListTip tooltipStates ]
        , blocks =
            [ Accordion.block [] [ viewSellingConfiguration sellingConfiguration ] ]
        }


viewSellingConfiguration : SellingConfiguration -> CardBlock.Item Msg
viewSellingConfiguration sellingConfiguration =
    CardBlock.custom <|
        Html.div []
            [ Radio.radio
                [ Radio.id "sc1"
                , radioName
                , Radio.checked (toEnum sellingConfiguration == SNothing)
                , Radio.onClick (SellingConfigChanged SellNothing)
                ]
                "Zakázat prodej participací"
            , Radio.radio
                [ Radio.id "sc2"
                , radioName
                , Radio.checked (toEnum sellingConfiguration == SWithoutCharge)
                , Radio.onClick (SellingConfigChanged SellWithoutCharge)
                ]
                "Prodávat všechny participace bez poplatku, které odpovídají filtrům tržiště"
            , Radio.radio
                [ Radio.id "sc3"
                , radioName
                , Radio.checked (toEnum sellingConfiguration == SWithoutChargeAndDiscount)
                , Radio.onClick (SellingConfigChanged SellWithoutChargeAndDiscount)
                ]
                "Prodávat všechny participace bez poplatku a slevy, které odpovídají filtrům tržiště."
            , Radio.radio
                [ Radio.id "sc4"
                , radioName
                , Radio.checked (toEnum sellingConfiguration == SSomething)
                , Radio.onClick <|
                    case sellingConfiguration of
                        SellSomething _ ->
                            -- Avoid filter deletion confirmation modal when user accidentally selects this radio when already selected
                            NoOp

                        _ ->
                            SellingConfigChanged (SellSomething [])
                ]
                "Prodávat participace splňující pravidla"
            , filterListControls sellingConfiguration
            ]


radioName : Radio.Option msg
radioName =
    Radio.name "sellconfig"


filterListControls : SellingConfiguration -> Html Msg
filterListControls sellingConfiguration =
    case sellingConfiguration of
        SellSomething filters ->
            Html.div []
                [ filterListView RemoveSellFilter filters
                , filterCreationButtons
                , Util.viewErrors <| validateSellingConfiguration sellingConfiguration
                ]

        SellWithoutChargeAndDiscount ->
            Html.text ""

        SellWithoutCharge ->
            Html.text ""

        SellNothing ->
            Html.text ""


filterCreationButtons : Html Msg
filterCreationButtons =
    Html.div []
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
        [ Html.text <| complexityButtonLabel filterComplexity ]


type SellConfigEnum
    = SNothing
    | SSomething
    | SWithoutCharge
    | SWithoutChargeAndDiscount


toEnum : SellingConfiguration -> SellConfigEnum
toEnum sellingConfiguration =
    case sellingConfiguration of
        SellSomething _ ->
            SSomething

        SellWithoutCharge ->
            SWithoutCharge

        SellWithoutChargeAndDiscount ->
            SWithoutChargeAndDiscount

        SellNothing ->
            SNothing
