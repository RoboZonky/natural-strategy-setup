module View.PortfolioStructure.PieChart exposing (viewChart)

import AllDict
import Array exposing (Array)
import Data.Filter.Conditions.Rating as Rating
import Data.PortfolioStructure exposing (PortfolioShares)
import Html exposing (Html)
import RangeSlider
import Svg exposing (Svg, circle, g, path, svg, text, text_)
import Svg.Attributes exposing (cx, cy, d, dominantBaseline, fill, height, r, stroke, transform, width, x, y)
import Visualization.Shape as Shape exposing (defaultPieConfig)


colors : Array String
colors =
    -- Pie slice colors taken from https://zonky.cz/otazky-a-odpovedi-investor
    Array.fromList
        [ "#596abe"
        , "#599ebe"
        , "#59bea8"
        , "#67cd75"
        , "#9acd67"
        , "#cebe5a"
        , "#d7954b"
        , "#e75637"
        , "lightgray"
        ]


extractMinimumShares : PortfolioShares -> List ( String, Float )
extractMinimumShares shares =
    let
        rtgLabelToMinPercentage =
            AllDict.toList shares
                |> List.map
                    (\( rtg, range ) ->
                        ( Rating.ratingToString rtg
                        , Tuple.first <| RangeSlider.getValues range
                        )
                    )

        remaining =
            100 - List.sum (List.map Tuple.second rtgLabelToMinPercentage)
    in
    rtgLabelToMinPercentage
        ++ [ ( "Neurčeno", remaining ) ]


viewChart : PortfolioShares -> Html msg
viewChart shares =
    let
        pieData =
            extractMinimumShares shares

        pieChart =
            pieData
                |> List.map Tuple.second
                |> Shape.pie { defaultPieConfig | sortingFn = \_ _ -> EQ }
                |> List.indexedMap makeSlice
                |> g [ transform "translate(110,110)" ]

        legend =
            pieData
                |> List.map2 (\color ( rtg, val ) -> ( rtg, val, color )) (Array.toList colors)
                |> List.filter (\( _, val, _ ) -> val >= 1)
                |> List.indexedMap makeLegend
                |> g [ transform "translate(250,25)" ]
    in
    svg [ width "400px", height "220px" ]
        [ pieChart
        , legend
        ]


makeLegend : Int -> ( String, Float, String ) -> Svg a
makeLegend index ( ratingStr, value, color ) =
    g []
        [ circle [ r "0.4em", cx "0", cy (toString index ++ "em"), fill color ] []
        , text_ [ x "1em", y (toString index ++ "em"), dominantBaseline "central" ]
            [ text <| ratingStr ++ "(" ++ toString (round value) ++ "%)" ]
        ]


makeSlice : Int -> Shape.Arc -> Svg a
makeSlice index datum =
    path
        [ d <| Shape.arc datum
        , fill <| Maybe.withDefault "#000" <| Array.get index colors
        , stroke "#fff"
        ]
        []
