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
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioShare, PortfolioShares)
import Data.Tooltip as Tooltip
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, selected, style, value)
import Html.Events exposing (onSubmit)
import RangeSlider
import Types exposing (..)
import Util
import View.PortfolioStructure.PieChart exposing (viewChart)
import View.Tooltip as Tooltip


form : Portfolio -> PortfolioShares -> Tooltip.States -> Accordion.Card Msg
form portfolio shares tooltipStates =
    let
        ( sharesTableOrSliders, contentDescription ) =
            case portfolio of
                Empty ->
                    ( portfolioSharesSliders shares, "Nastavte" )

                _ ->
                    ( portfolioSharesTable shares, "Následující tabulka ukazuje" )
    in
    Accordion.card
        { id = "portfolioStructureCard"
        , options = []
        , header =
            Accordion.headerH4 [] (Accordion.toggle [] [ text "Struktura portfolia" ])
                |> Accordion.appendHeader [ Tooltip.popoverTip Tooltip.portfolioStructureTip tooltipStates ]
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
                                [ viewChart shares ]
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
portfolioShareRow ( rtg, share ) =
    let
        ( mi, mx ) =
            PortfolioStructure.toIntRange share
    in
    Table.tr []
        [ Table.td [] [ text <| Rating.ratingToString rtg ]
        , Table.td [] [ text <| toString mi ]
        , Table.td [] [ text <| toString mx ]
        ]


portfolioSharesSliders : PortfolioShares -> Html Msg
portfolioSharesSliders shares =
    let
        validationErrors =
            Util.viewErrors <| PortfolioStructure.validate shares
    in
    Dict.toList shares
        |> List.map
            (\( rating, sliderState ) ->
                Form.formInline [ onSubmit NoOp ]
                    [ div [ style [ ( "width", "50px" ) ] ] [ text <| ratingToString rating ]
                    , Html.map (ChangePortfolioSharePercentage rating) <| RangeSlider.view sliderState
                    ]
            )
        |> (\sliders -> div [] (sliders ++ [ validationErrors ]))
