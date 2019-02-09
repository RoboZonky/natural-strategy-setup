module Data.PortfolioStructure exposing
    ( PortfolioShare
    , PortfolioShares
    , decoder
    , encode
    , percentageShare
    , portfolioSharesEqual
    , portfolioSlidersSubscription
    , renderPortfolioShares
    , toIntRange
    , validate
    )

import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.Portfolio exposing (Portfolio(..))
import Data.SharedJsonStuff
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RangeSlider exposing (RangeSlider, setDimensions, setExtents, setFormatter, setStepSize, setValues)
import Types
import Util


type alias PortfolioShares =
    AnyDict Int Rating Share


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
        String.fromInt minPercent

    else
        String.fromInt minPercent ++ " až " ++ String.fromInt maxPercent


renderPortfolioShares : Portfolio -> PortfolioShares -> String
renderPortfolioShares portfolio shares =
    case portfolio of
        UserDefined ->
            Dict.Any.toList shares
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
        |> setFormatter (\value -> String.fromFloat value ++ "%")
        |> setDimensions 300 57
        |> setExtents 0 100
        |> setValues (toFloat from) (toFloat to)


portfolioSlidersSubscription : PortfolioShares -> Sub Types.Msg
portfolioSlidersSubscription shares =
    Dict.Any.toList shares
        |> List.map (\( rtg, sliderState ) -> Sub.map (Types.ChangePortfolioSharePercentage rtg) (RangeSlider.subscriptions sliderState))
        |> Sub.batch


validate : PortfolioShares -> List String
validate portfolioShares =
    let
        sumOfShareMinimums =
            Dict.Any.foldr (\_ sliderState sumAcc -> sumAcc + getSliderMinimum sliderState) 0 portfolioShares
    in
    Util.validate (sumOfShareMinimums /= 100) <|
        "Součet minim musí být přesně 100% (teď je "
            ++ String.fromInt sumOfShareMinimums
            ++ "%)"


getSliderMinimum : RangeSlider -> Int
getSliderMinimum =
    round << Tuple.first << RangeSlider.getValues


portfolioSharesEqual : PortfolioShares -> PortfolioShares -> Bool
portfolioSharesEqual ps1 ps2 =
    let
        getSliderValues =
            Dict.Any.values >> List.map RangeSlider.getValues
    in
    getSliderValues ps1 == getSliderValues ps2



-- JSON


encode : PortfolioShares -> Value
encode =
    Data.SharedJsonStuff.encodeRatingToSliderDict encodeShare


decoder : Decoder PortfolioShares
decoder =
    Data.SharedJsonStuff.ratingToSliderDictDecodr shareDecoder


encodeShare : Share -> Value
encodeShare sz =
    toIntRange sz |> (\( from, to ) -> Encode.list Encode.int [ from, to ])


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
