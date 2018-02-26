module View.SellConfig exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter as Filter exposing (FilteredItem(Participation_To_Sell), MarketplaceFilter, SellConf(..), SellingConfiguration(..), renderSellFilter)
import Data.Tooltip as Tooltip
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (CreationModalMsg(ModalStateMsg), Msg(CreationModalMsg, RemoveSellFilter, SetSellingConfiguration))
import Util
import View.Tooltip as Tooltip


form : SellingConfiguration -> Tooltip.States -> Accordion.Card Msg
form sellingConfiguration tooltipStates =
    Accordion.card
        { id = "sellingConfigCard"
        , options = []
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
                , filtersView filters
                , Util.viewErrors <| Filter.validateSellingConfiguration sellingConfiguration
                ]

        SellNothing ->
            text ""


filtersView : List MarketplaceFilter -> Html Msg
filtersView filters =
    div [ Spacing.p2 ] <| List.indexedMap viewFilter filters


viewFilter : Int -> MarketplaceFilter -> Html Msg
viewFilter index mf =
    let
        removeButton =
            span [ onClick (RemoveSellFilter index), class "float-right" ] [ text "✖" ]

        filterText =
            span [] [ text <| renderSellFilter mf ]
    in
    Card.config []
        |> Card.block [ CardBlock.attrs [ Spacing.p2 ] ]
            [ CardBlock.custom <|
                Grid.row []
                    [ Grid.col [ Col.xs11 ] [ filterText ]
                    , Grid.col [ Col.xs1 ] [ removeButton ]
                    ]
            ]
        |> Card.view


filterCreationControls : Html Msg
filterCreationControls =
    div []
        [ Button.button
            [ Button.primary
            , Button.onClick <| CreationModalMsg <| ModalStateMsg Participation_To_Sell Modal.shown
            , Button.attrs [ Spacing.mx1 ]
            , Button.small
            ]
            [ text "Přidat pravidlo" ]
        ]
