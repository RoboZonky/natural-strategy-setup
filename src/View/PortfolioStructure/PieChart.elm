module View.PortfolioStructure.PieChart exposing (viewChart)

import AllDict
import Array exposing (Array)
import Data.Filter.Conditions.Rating as Rating
import Data.PortfolioStructure exposing (PortfolioShares)
import Html exposing (Html, br, div, h2, input, label)
import RangeSlider
import Svg exposing (Svg, g, path, svg, text, text_, tspan)
import Svg.Attributes exposing (d, dy, fill, height, stroke, style, textAnchor, transform, width, x)
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


sharesToPieData : PortfolioShares -> List ( String, Float )
sharesToPieData shares =
    let
        rtgLabelToMinPercentage =
            AllDict.toList shares |> List.map (\( rtg, range ) -> ( Rating.ratingToString rtg, Tuple.first <| RangeSlider.getValues range ))

        remaining =
            100 - List.sum (List.map Tuple.second rtgLabelToMinPercentage)
    in
    rtgLabelToMinPercentage
        ++ [ ( "Neurčeno", remaining ) ]
        |> List.map
            (\( lbl, val ) ->
                ( if val < 1 then
                    ""
                  else
                    lbl
                , val
                )
            )


viewChart : PortfolioShares -> Html msg
viewChart shares =
    let
        labelPosition =
            120

        model =
            sharesToPieData shares

        pieData =
            model
                |> List.map Tuple.second
                |> Shape.pie
                    { defaultPieConfig
                        | sortingFn = \_ _ -> EQ
                    }

        makeSlice index datum =
            path [ d (Shape.arc datum), fill (Maybe.withDefault "#000" <| Array.get index colors), stroke "#fff" ] []

        makeLabel slice ( label, value ) =
            text_
                [ transform ("translate" ++ toString (Shape.centroid { slice | innerRadius = labelPosition, outerRadius = labelPosition }))
                , textAnchor "middle"
                ]
                [ tspan [ x "0" ] [ text label ]
                , tspan [ x "0", dy "1em" ]
                    [ text <|
                        if value < 1 then
                            ""
                        else
                            toString (round value) ++ "%"
                    ]
                ]
    in
    svg [ width "300px", height "300px" ]
        [ g [ transform "translate(150,150)" ]
            [ g [] <| List.indexedMap makeSlice pieData
            , g [] <| List.map2 makeLabel pieData model
            ]
        ]
