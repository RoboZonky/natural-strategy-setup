module View.SellConfig exposing (Config, form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter exposing (FilteredItem(..), SellingConfiguration(..), validateSellingConfiguration)
import Data.Filter.Complexity exposing (FilterComplexity(..), complexityButtonLabel)
import Data.Tooltip as Tooltip
import Html exposing (Html)
import Util
import Version exposing (filtersHowToLink)
import View.CardHeightWorkaround exposing (markOpenedAccordionCard)
import View.Filter exposing (filterListView)
import View.Tooltip as Tooltip


type alias Config msg =
    { tooltipConfig : Tooltip.Config msg
    , sellingConfigChanged : SellingConfiguration -> msg
    , removeSellFilter : Int -> msg
    , openCreationModal : FilterComplexity -> List FilteredItem -> msg
    , noOp : msg
    }


form : Config msg -> SellingConfiguration -> Accordion.State -> Tooltip.States -> Accordion.Card msg
form config sellingConfiguration accordionState tooltipStates =
    let
        cardId =
            "sellingConfigCard"
    in
    Accordion.card
        { id = cardId
        , options = [ markOpenedAccordionCard cardId accordionState ]
        , header =
            Accordion.headerH4 [] (Accordion.toggle [] [ Html.text "Pravidla prodeje" ])
                |> Accordion.appendHeader [ Tooltip.popoverTip config.tooltipConfig Tooltip.sellFilterListTip tooltipStates ]
        , blocks =
            [ Accordion.block [] [ viewSellingConfiguration config sellingConfiguration ] ]
        }


viewSellingConfiguration : Config msg -> SellingConfiguration -> CardBlock.Item msg
viewSellingConfiguration config sellingConfiguration =
    CardBlock.custom <|
        Html.div []
            [ Radio.radio
                [ Radio.id "sc1"
                , radioName
                , Radio.checked (toEnum sellingConfiguration == SNothing)
                , Radio.onClick (config.sellingConfigChanged SellNothing)
                ]
                "Zakázat prodej participací"
            , Radio.radio
                [ Radio.id "sc2"
                , radioName
                , Radio.checked (toEnum sellingConfiguration == SWithoutCharge)
                , Radio.onClick (config.sellingConfigChanged SellWithoutCharge)
                ]
                "Prodávat všechny participace bez poplatku, které odpovídají filtrům tržiště"
            , Radio.radio
                [ Radio.id "sc3"
                , radioName
                , Radio.checked (toEnum sellingConfiguration == SWithoutChargeAndDiscount)
                , Radio.onClick (config.sellingConfigChanged SellWithoutChargeAndDiscount)
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
                            config.noOp

                        _ ->
                            config.sellingConfigChanged (SellSomething [])
                ]
                "Prodávat participace splňující pravidla"
            , filterListControls config sellingConfiguration
            ]


radioName : Radio.Option msg
radioName =
    Radio.name "sellconfig"


filterListControls : Config msg -> SellingConfiguration -> Html msg
filterListControls config sellingConfiguration =
    case sellingConfiguration of
        SellSomething filters ->
            Html.div []
                [ filterListView config.removeSellFilter filters
                , filterCreationButtons config
                , Util.viewErrors <| validateSellingConfiguration sellingConfiguration
                ]

        SellWithoutChargeAndDiscount ->
            Html.text ""

        SellWithoutCharge ->
            Html.text ""

        SellNothing ->
            Html.text ""


filterCreationButtons : Config msg -> Html msg
filterCreationButtons config =
    Html.div []
        [ filterCreationButton config Simple Button.primary
        , filterCreationButton config Complex Button.outlineSecondary
        , filtersHowToLink
        ]


filterCreationButton : Config msg -> FilterComplexity -> Button.Option msg -> Html msg
filterCreationButton config filterComplexity buttonType =
    Button.button
        [ buttonType
        , Button.onClick <| config.openCreationModal filterComplexity [ Participation_To_Sell ]
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
