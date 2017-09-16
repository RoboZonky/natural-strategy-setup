module View.PortfolioStructure exposing (defaultPortfolioForm, form)

import AllDict as Dict
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table
import Data.Filter.Conditions.Rating as Rating exposing (ratingToString)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure exposing (PortfolioShare, PortfolioShares, Share)
import Data.Tooltip as Tooltip
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class, selected, size, style, value)
import Html.Events exposing (onSubmit)
import Plot
import RangeSlider
import Slider exposing (SliderStates)
import Types exposing (..)
import View.Tooltip as Tooltip


form : Portfolio -> PortfolioShares -> Tooltip.States -> SliderStates -> Accordion.Card Msg
form portfolio shares tooltipStates sliderStates =
    let
        ( sharesTableOrSliders, contentDescription ) =
            case portfolio of
                Empty ->
                    ( portfolioSharesSliders sliderStates, "Nastavte" )

                _ ->
                    ( portfolioSharesTable shares, "Následující tabulka ukazuje" )
    in
    Accordion.card
        { id = "portfolioStructureCard"
        , options = []
        , header =
            Accordion.headerH4 [] <|
                Accordion.toggle []
                    [ text "Struktura portfolia"
                    , Tooltip.popoverTip Tooltip.portfolioStructureTip tooltipStates
                    ]
        , blocks =
            [ Accordion.block []
                [ Card.custom <|
                    div []
                        [ defaultPortfolioForm portfolio
                        , text <| contentDescription ++ " požadovaný procentuální podíl aktuální zůstatkové částky investovaný do půjček v daném ratingu"
                        , Grid.row []
                            [ Grid.col [ Col.xs6 ]
                                [ sharesTableOrSliders ]
                            , Grid.col [ Col.xs6 ]
                                [ plotShares shares ]
                            ]
                        ]
                ]
            ]
        }


plotShares : PortfolioShares -> Html Msg
plotShares shares =
    Plot.viewBars
        (Plot.groups (List.map (\( rtg, ( mi, ma ) ) -> Plot.group (ratingToString rtg) [ toFloat mi, toFloat ma ])))
        (Dict.toList shares)


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
                        [ value (toString portfolio), selected (portfolio == currentPortfolio) ]
                        [ text (Portfolio.toString portfolio) ]
                )
                [ Conservative, Balanced, Progressive, Empty ]
    in
    Select.select
        [ Select.small
        , Select.onChange (PortfolioChanged << Portfolio.fromString)
        , Select.attrs [ class "mx-1" ]
        ]
        optionList


portfolioSharesTable : PortfolioShares -> Html Msg
portfolioSharesTable shares =
    Table.table
        { options = [ Table.bordered, Table.small ]
        , thead =
            Table.thead [ Table.defaultHead ]
                [ Table.tr []
                    [ Table.th [] [ text "Rating" ]
                    , Table.th [] [ text "Od (%)" ]
                    , Table.th [] [ text "Do (%)" ]
                    ]
                ]
        , tbody = Table.tbody [] <| List.map portfolioShareRow <| Dict.toList shares
        }


portfolioShareRow : PortfolioShare -> Table.Row Msg
portfolioShareRow ( rtg, ( mi, mx ) ) =
    Table.tr []
        [ Table.td [] [ text <| Rating.ratingToString rtg ]
        , Table.td [] [ text <| toString mi ]
        , Table.td [] [ text <| toString mx ]
        ]


portfolioSharesSliders : SliderStates -> Html Msg
portfolioSharesSliders sliderStates =
    let
        sumOfShareMinimums =
            Dict.foldr (\_ sliderState sumAcc -> sumAcc + round (Tuple.first <| RangeSlider.getValues sliderState)) 0 sliderStates

        validationError =
            if sumOfShareMinimums /= 100 then
                [ div [ style [ ( "color", "red" ) ] ]
                    [ text <| "Součet minim musí být přesně 100% (teď je " ++ toString sumOfShareMinimums ++ "%)" ]
                ]
            else
                []
    in
    Dict.toList sliderStates
        |> List.map
            (\( rating, sliderState ) ->
                Form.formInline [ onSubmit NoOp ]
                    [ div [ style [ ( "width", "50px" ) ] ] [ text <| ratingToString rating ]
                    , Html.map (ChangePortfolioSharePercentage rating) <| RangeSlider.view sliderState
                    ]
            )
        |> (\sliders -> div [] (sliders ++ validationError))
