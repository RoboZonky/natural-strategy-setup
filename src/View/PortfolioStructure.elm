module View.PortfolioStructure exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter.Conditions.Rating as Rating exposing (ratingDictToList)
import Data.Portfolio as Portfolio exposing (Portfolio(..), allPortfolios)
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.Tooltip as Tooltip
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, selected, style, value)
import Html.Events exposing (onSubmit)
import Percentage
import Types exposing (Msg(..))
import View.CardHeightWorkaround exposing (markOpenedAccordionCard)
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
                        , Html.p [] [ text "Požadovaný podíl investovaný do půjček podle rizikových kategorií můžete upravit pomocí posuvníků" ]
                        , portfolioSharesSliders shares
                        , sumSummaryView shares
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


sumSummaryView : PortfolioShares -> Html a
sumSummaryView shares =
    let
        sumOfShares =
            PortfolioStructure.shareSum shares

        warnings =
            if sumOfShares < 100 then
                Html.p [ style "color" "red" ]
                    [ Html.text "Součet podílů nesmí být menší než 100%" ]

            else if sumOfShares > 100 then
                Html.p [ style "color" "orange" ]
                    -- TODO this shows 101 for conservative - should I enable
                    [ text "Součet podílů přesahuje 100%, což není nutně problém, ale může vést k nepředvídatelné struktuře portfolia." ]

            else
                Html.div [ style "height" "24px" ] []
    in
    Html.p []
        [ text "Součet podílů je "
        , Html.b [] [ Html.text <| String.fromInt (PortfolioStructure.shareSum shares) ++ " %" ]
        , warnings
        ]


portfolioSharesSliders : PortfolioShares -> Html Msg
portfolioSharesSliders shares =
    let
        ratingSlider ( rating, percentage ) =
            Form.formInline [ onSubmit NoOp, class (Rating.toColorClass rating) ]
                [ Html.b [ style "width" "105px" ] [ text <| Rating.showInterestPercent rating ]
                , Html.map (ChangePortfolioSharePercentage rating) <| Percentage.view percentage
                , Html.b [ Spacing.mx2 ] [ text <| String.fromInt (Percentage.toInt percentage) ++ " %" ]
                ]
    in
    ratingDictToList shares
        |> List.map ratingSlider
        |> Html.p []
