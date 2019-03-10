module View.PortfolioStructure.BarChart exposing (view)

import Array exposing (Array)
import Data.Filter.Conditions.Rating as Rating exposing (Rating, ratingDictToList)
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
            createBarWidthCalculator <| getBiggestMaximumShare portfolioShares

        viewBar : Int -> BarData -> Svg msg
        viewBar =
            makeBar barWidthCalculator

        bars =
            List.indexedMap viewBar barData
    in
    svg
        [ width "400px"
        , height "365px"
        ]
        bars


makeBar : (Float -> Float) -> Int -> BarData -> Svg a
makeBar barWidthCalculator index { rating, sharePercentMin, sharePercentMax, color } =
    let
        yPos =
            20 + index * 36

        minimumBarWidth =
            barWidthCalculator sharePercentMin

        maximumBarWidth =
            barWidthCalculator sharePercentMax

        barHeight =
            18

        legend =
            String.fromFloat sharePercentMin
                ++ (if minimumBarWidth == maximumBarWidth then
                        ""

                    else
                        "-" ++ String.fromFloat sharePercentMax
                   )
                ++ "%"
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
            , width <| String.fromFloat minimumBarWidth
            , fill color
            ]
            []
        , rect
            [ x "70"
            , y <| String.fromInt yPos
            , height <| String.fromInt barHeight
            , width <| String.fromFloat maximumBarWidth
            , fill color
            , style "fill-opacity:0.5"
            ]
            []
        , text_
            [ x <| String.fromFloat <| maximumBarWidth + 75
            , y <| String.fromInt <| yPos + (barHeight // 2)
            , dominantBaseline "central"
            ]
            [ text legend ]
        ]


type alias BarData =
    { rating : Rating
    , sharePercentMin : Float
    , sharePercentMax : Float
    , color : Color
    }


getBarData : PortfolioShares -> List BarData
getBarData shares =
    List.map2
        (\( rating, slider ) color ->
            let
                ( mi, ma ) =
                    RangeSlider.getValues slider
            in
            { rating = rating
            , sharePercentMin = mi
            , sharePercentMax = ma
            , color = color
            }
        )
        (ratingDictToList shares)
        (Array.toList colors)


{-| Each bar represents a percentage (0-100%).

When the percentages are small use fixed pixel width per each share percent
(so the bars are not too small).

When the percentages are larger, use proportion of maximum share in the graph
(so the bars don't overflow the form)

-}
createBarWidthCalculator : Float -> (Float -> Float)
createBarWidthCalculator biggestMaximumShare =
    let
        pixelsPerPercent =
            if biggestMaximumShare < 30 then
                10

            else
                260 / biggestMaximumShare
    in
    \sharePercent -> pixelsPerPercent * sharePercent


getBiggestMaximumShare : PortfolioShares -> Float
getBiggestMaximumShare portfolioShares =
    Dict.Any.values portfolioShares
        |> List.map (RangeSlider.getValues >> Tuple.second)
        |> List.maximum
        |> Maybe.withDefault 0


type alias Color =
    String


colors : Array Color
colors =
    -- Pie slice colors taken from zonky dashboard
    Array.fromList
        [ "#8b59be" -- AAAAA
        , "#596abe" -- AAAA
        , "#599ebe" -- AAA
        , "#5ABFA9" -- AAE
        , "#67cd75" -- AA
        , "#91C95A" -- AE
        , "#cebe5a" -- A
        , "#d7954b" -- B
        , "#e75637" -- C
        , "#d12f2f" -- D
        ]
