module View.PortfolioStructure.PieChart exposing (viewChart)

import Array exposing (Array)
import Data.Filter.Conditions.Rating as Rating
import Data.PortfolioStructure exposing (PortfolioShares)
import Dict.Any
import Html exposing (Html)
import Path
import RangeSlider
import Shape exposing (defaultPieConfig)
import Svg exposing (Svg, circle, g, svg, text, text_)
import Svg.Attributes exposing (cx, cy, dominantBaseline, fill, height, r, stroke, transform, width, x, y)


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
            Dict.Any.toList shares
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
        [ circle [ r "0.4em", cx "0", cy (String.fromInt index ++ "em"), fill color ] []
        , text_ [ x "1em", y (String.fromInt index ++ "em"), dominantBaseline "central" ]
            [ text <| ratingStr ++ "(" ++ String.fromInt (round value) ++ "%)" ]
        ]


makeSlice : Int -> Shape.Arc -> Svg a
makeSlice index arc =
    Path.element (Shape.arc arc)
        [ fill <| Maybe.withDefault "#000" <| Array.get index colors
        , stroke "#fff"
        ]
