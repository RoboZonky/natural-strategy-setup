module View.PortfolioStructure exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter.Conditions.Rating as Rating exposing (ratingDictToList)
import Data.Portfolio as Portfolio exposing (Portfolio(..), allPortfolios)
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.Tooltip as Tooltip
import Html exposing (Html, b, div, text)
import Html.Attributes exposing (class, selected, style, value)
import Html.Events exposing (onSubmit)
import RangeSlider
import Types exposing (Msg(..))
import Util
import View.CardHeightWorkaround exposing (markOpenedAccordionCard)
import View.PortfolioStructure.BarChart as BarChart
import View.Tooltip as Tooltip


form : Portfolio -> PortfolioShares -> Accordion.State -> Tooltip.States -> Accordion.Card Msg
form portfolio shares accordionState tooltipStates =
    let
        cardId =
            "portfolioStructureCard"
    in
    Accordion.card
        { id = cardId
        , options = [ markOpenedAccordionCard cardId accordionState ]
        , header =
            Accordion.headerH4 [] (Accordion.toggle [] [ text "Struktura portfolia" ])
                |> Accordion.appendHeader [ Tooltip.popoverTip Tooltip.portfolioStructureTip tooltipStates ]
        , blocks =
            [ Accordion.block []
                [ CardBlock.custom <|
                    div [ class "tab-with-sliders" ]
                        [ defaultPortfolioForm portfolio
                        , text "Požadovaný podíl investovaný do půjček podle rizikových kategorií můžete upravit pomocí posuvníků"
                        , Grid.row []
                            [ Grid.col [ Col.xs6 ]
                                [ portfolioSharesSliders shares ]
                            , Grid.col [ Col.xs6 ]
                                [ BarChart.view shares
                                , validationErrors shares
                                ]
                            ]
                        ]
                ]
            ]
        }


defaultPortfolioForm : Portfolio -> Html Msg
defaultPortfolioForm currentPortfolio =
    Form.formInline [ onSubmit NoOp ]
        [ text "Robot má udržovat ", defaultPortfolioSelect currentPortfolio, text " portfolio." ]


defaultPortfolioSelect : Portfolio -> Html Msg
defaultPortfolioSelect currentPortfolio =
    let
        optionList =
            List.map
                (\portfolio ->
                    Select.item
                        [ value (Portfolio.toString portfolio), selected (portfolio == currentPortfolio) ]
                        [ text (Portfolio.toUiLabel portfolio) ]
                )
                allPortfolios
    in
    Select.select
        [ Select.small
        , Select.onChange (PortfolioChanged << Portfolio.fromString)
        , Select.attrs [ Spacing.mx1 ]
        ]
        optionList


validationErrors : PortfolioShares -> Html a
validationErrors shares =
    case PortfolioStructure.validate shares of
        [] ->
            text ""

        errors ->
            Util.viewErrors errors


portfolioSharesSliders : PortfolioShares -> Html Msg
portfolioSharesSliders shares =
    let
        ratingSlider ( rating, sliderState ) =
            Form.formInline [ onSubmit NoOp ]
                [ b [ style "width" "105px" ] [ text <| Rating.showInterestPercent rating ]
                , Html.map (ChangePortfolioSharePercentage rating) <| RangeSlider.view sliderState
                ]
    in
    ratingDictToList shares
        |> List.map ratingSlider
        |> div []
