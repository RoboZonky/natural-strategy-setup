module Data.PortfolioStructure
    exposing
        ( PortfolioShare
        , PortfolioShares
        , Share
        , decoder
        , encode
        , percentageShare
        , portfolioSlidersSubscription
        , renderPortfolioShare
        , renderPortfolioShares
        , toIntRange
        , validate
        )

import AllDict exposing (AllDict)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.Portfolio exposing (Portfolio(Empty))
import Data.SharedJsonStuff
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
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


validate : PortfolioShares -> List String
validate portfolioShares =
    let
        sumOfShareMinimums =
            AllDict.foldr (\_ sliderState sumAcc -> sumAcc + round (Tuple.first <| RangeSlider.getValues sliderState)) 0 portfolioShares
    in
    Util.validate (sumOfShareMinimums /= 100) <| "Součet minim musí být přesně 100% (teď je " ++ toString sumOfShareMinimums ++ "%)"



-- JSON


encode : PortfolioShares -> Value
encode =
    Data.SharedJsonStuff.encodeRatingToSliderDict encodeShare


decoder : Decoder PortfolioShares
decoder =
    Data.SharedJsonStuff.ratingToSliderDictDecodr shareDecoder


encodeShare : Share -> Value
encodeShare sz =
    toIntRange sz |> (\( from, to ) -> Encode.list [ Encode.int from, Encode.int to ])


shareDecoder : Decoder Share
shareDecoder =
    Decode.list Decode.int
        |> Decode.map
            (\xs ->
                case xs of
                    from :: to :: [] ->
                        percentageShare from to

                    _ ->
                        percentageShare 200 200
            )
