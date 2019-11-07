module View.PortfolioStructure.BarChart exposing (view)

import Array exposing (Array)
import Data.Filter.Conditions.Rating as Rating exposing (Rating, ratingDictToList)
import Data.PortfolioStructure exposing (PortfolioShares)
import Dict.Any
import Html exposing (Html)
import Percentage
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
            createBarWidthCalculator <| getBiggestPercentage portfolioShares

        viewBar : Int -> BarData -> Svg msg
        viewBar =
            makeBar barWidthCalculator

        bars : List (Svg msg)
        bars =
            List.indexedMap viewBar barData

        chartHeightPx : String
        chartHeightPx =
            String.fromInt (pxSpacePerBar * List.length Rating.allRatings) ++ "px"
    in
    svg
        [ width "400px"
        , height chartHeightPx
        ]
        bars


makeBar : (Float -> Float) -> Int -> BarData -> Svg a
makeBar barWidthCalculator index { rating, sharePercent, color } =
    let
        yPos =
            10 + index * pxSpacePerBar

        barWidth =
            barWidthCalculator sharePercent

        legend =
            String.fromFloat sharePercent ++ "%"
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
            [ text legend ]
        ]


type alias BarData =
    { rating : Rating
    , sharePercent : Float
    , color : Color
    }


getBarData : PortfolioShares -> List BarData
getBarData shares =
    List.map2
        (\( rating, percentage ) color ->
            { rating = rating
            , sharePercent = toFloat <| Percentage.toInt percentage
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
createBarWidthCalculator biggestPercentage =
    let
        pixelsPerPercent =
            if biggestPercentage < 30 then
                10

            else
                260 / biggestPercentage
    in
    \sharePercent -> pixelsPerPercent * sharePercent


getBiggestPercentage : PortfolioShares -> Float
getBiggestPercentage portfolioShares =
    Dict.Any.values portfolioShares
        |> List.map (toFloat << Percentage.toInt)
        |> List.maximum
        |> Maybe.withDefault 0


type alias Color =
    String


colors : Array Color
colors =
    -- Pie slice colors taken from zonky dashboard
    Array.fromList
        [ "#c0498b" -- AAAAAA
        , "#8b59be" -- AAAAA
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


{-| Includes bar height and some space around it
-}
pxSpacePerBar : Int
pxSpacePerBar =
    barHeight * 2


barHeight : Int
barHeight =
    18
