module Data.PortfolioStructure
    exposing
        ( PortfolioShare
        , PortfolioShares
        , Share
        , percentageShare
        , portfolioSlidersSubscription
        , renderPortfolioShare
        , renderPortfolioShares
        , toIntRange
        )

import AllDict exposing (AllDict)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.Portfolio exposing (Portfolio(Empty))
import RangeSlider exposing (RangeSlider, setDimensions, setExtents, setFormatter, setStepSize, setValues)
import Types
import Util


type alias PortfolioShares =
    AllDict Rating Share Int


type alias PortfolioShare =
    ( Rating, Share )


type alias Share =
    RangeSlider


renderPortfolioShare : PortfolioShare -> String
renderPortfolioShare ( rating, share ) =
    "Prostředky v ratingu " ++ Rating.ratingToString rating ++ " tvoří " ++ renderShare share ++ " % aktuální zůstatkové částky."


renderShare : Share -> String
renderShare share =
    let
        ( minPercent, maxPercent ) =
            toIntRange share
    in
    if minPercent == maxPercent then
        toString minPercent
    else
        toString minPercent ++ " až " ++ toString maxPercent


renderPortfolioShares : Portfolio -> PortfolioShares -> String
renderPortfolioShares portfolio shares =
    case portfolio of
        -- Only render this section when user "overrides" predefined DefaultPortfolios
        Empty ->
            AllDict.toList shares
                -- Only render share in the config when maximum > 0
                |> List.filter (\( _, share ) -> Tuple.second (toIntRange share) > 0)
                |> List.map renderPortfolioShare
                |> Util.renderNonemptySection "\n- Úprava struktury portfolia"

        _ ->
            ""


toIntRange : Share -> ( Int, Int )
toIntRange =
    RangeSlider.getValues >> (\( a, b ) -> ( round a, round b ))


percentageShare : Int -> Int -> Share
percentageShare from to =
    RangeSlider.init
        |> setStepSize (Just 1.0)
        |> setFormatter (\value -> toString value ++ "%")
        |> setDimensions 300 57
        |> setExtents 0 100
        |> setValues (toFloat from) (toFloat to)


portfolioSlidersSubscription : PortfolioShares -> Sub Types.Msg
portfolioSlidersSubscription shares =
    AllDict.toList shares
        |> List.map (\( rtg, sliderState ) -> Sub.map (Types.ChangePortfolioSharePercentage rtg) (RangeSlider.subscriptions sliderState))
        |> Sub.batch
