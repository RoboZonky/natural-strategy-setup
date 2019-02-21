module View.PortfolioStructure.BarChart exposing (view)

import Array exposing (Array)
import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Data.PortfolioStructure exposing (PortfolioShares)
import Dict.Any
import Html exposing (Html)
import RangeSlider
import Svg exposing (Svg, g, rect, svg, text, text_)
import Svg.Attributes exposing (dominantBaseline, fill, height, style, width, x, y)


view : PortfolioShares -> Html msg
view portfolioShares =
    let
        barData : List BarData
        barData =
            getBarData portfolioShares

        barWidthCalculator : Float -> Float
        barWidthCalculator =
            createBarWidthCalculator <| getBiggestMinimumShare portfolioShares

        viewBar : Int -> BarData -> Svg msg
        viewBar =
            makeBar barWidthCalculator

        bars =
            List.indexedMap viewBar barData
    in
    svg
        [ width "400px"
        , height "330px"
        ]
        bars


makeBar : (Float -> Float) -> Int -> BarData -> Svg a
makeBar barWidthCalculator index { rating, sharePercent, color } =
    let
        yPos =
            20 + index * 36

        barWidth =
            barWidthCalculator sharePercent

        barHeight =
            18
    in
    g []
        [ text_
            [ x "0"
            , y <| String.fromInt <| yPos + (barHeight // 2)
            , dominantBaseline "central"
            , style "font-weight:bold"
            ]
            [ text <| String.padLeft 5 '\u{00A0}' <| Rating.showInterest rating ++ " %" ]
        , rect
            [ x "70"
            , y <| String.fromInt yPos
            , height <| String.fromInt barHeight
            , width <| String.fromFloat barWidth
            , fill color
            ]
            []
        , text_
            [ x <| String.fromFloat <| barWidth + 75
            , y <| String.fromInt <| yPos + (barHeight // 2)
            , dominantBaseline "central"
            ]
            [ text <| String.fromFloat sharePercent ++ "%" ]
        ]


type alias BarData =
    { rating : Rating
    , sharePercent : Float
    , color : Color
    }


getBarData : PortfolioShares -> List BarData
getBarData shares =
    List.map2
        (\( rating, slider ) color ->
            { rating = rating
            , sharePercent = Tuple.first <| RangeSlider.getValues slider
            , color = color
            }
        )
        (Dict.Any.toList shares)
        (Array.toList colors)


{-| Each bar represents a percentage (0-100%).

When the percentages are small use fixed pixel width per each share percent
(so the bars are not too small).

When the percentages are larger, use proportion of maximum share in the graph
(so the bars don't overflow the form)

-}
createBarWidthCalculator : Float -> (Float -> Float)
createBarWidthCalculator biggestMinimumShare =
    let
        pixelsPerPercent =
            if biggestMinimumShare < 30 then
                10

            else
                280 / biggestMinimumShare
    in
    \sharePercent -> pixelsPerPercent * sharePercent


getBiggestMinimumShare : PortfolioShares -> Float
getBiggestMinimumShare portfolioShares =
    Dict.Any.values portfolioShares
        |> List.map (RangeSlider.getValues >> Tuple.first)
        |> List.maximum
        |> Maybe.withDefault 0


type alias Color =
    String


colors : Array Color
colors =
    -- Pie slice colors taken from zonky dashboard
    Array.fromList
        [ "#8b59be"
        , "#596abe"
        , "#599ebe"
        , "#67cd75"
        , "#cebe5a"
        , "#d7954b"
        , "#e75637"
        , "#d12f2f"
        ]
