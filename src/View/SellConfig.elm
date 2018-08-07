module View.SellConfig exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Radio as Radio
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter as Filter exposing (FilteredItem(Participation_To_Sell), SellConf(..), SellingConfiguration(..))
import Data.Tooltip as Tooltip
import Html exposing (Html, div, text)
import Types exposing (CreationModalMsg(ModalStateMsg), Msg(CreationModalMsg, RemoveSellFilter, SetSellingConfiguration))
import Util
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
            [ Accordion.block [] [ sellingConfigurationRadios sellingConfiguration ] ]
        }


sellingConfigurationRadios : SellingConfiguration -> CardBlock.Item Msg
sellingConfigurationRadios sellingConfiguration =
    CardBlock.custom <|
        div []
            [ sellingConfigurationRadio sellingConfiguration Filter.SNothing
            , sellingConfigurationRadio sellingConfiguration Filter.SSomething
            , viewSellingConfiguration sellingConfiguration
            ]


sellingConfigurationRadio : SellingConfiguration -> SellConf -> Html Msg
sellingConfigurationRadio currentConfiguration thisRadiosConf =
    Radio.radio
        [ Radio.id (toString thisRadiosConf)
        , Radio.name "sellingConfiguration"
        , Radio.checked <| Filter.toSellConfEnum currentConfiguration == thisRadiosConf
        , Radio.onClick (SetSellingConfiguration thisRadiosConf)
        ]
        (Filter.sellConfRadioLabel thisRadiosConf)


viewSellingConfiguration : SellingConfiguration -> Html Msg
viewSellingConfiguration sellingConfiguration =
    case sellingConfiguration of
        SellSomething filters ->
            div []
                [ filterCreationControls
                , filterListView RemoveSellFilter filters
                , Util.viewErrors <| Filter.validateSellingConfiguration sellingConfiguration
                ]

        SellNothing ->
            text ""


filterCreationControls : Html Msg
filterCreationControls =
    Button.button
        [ Button.primary
        , Button.onClick <| CreationModalMsg <| ModalStateMsg Participation_To_Sell Modal.shown
        , Button.attrs [ Spacing.mx1 ]
        , Button.small
        ]
        [ text "Přidat pravidlo" ]
