module Data.Investment exposing
    ( InvestmentsPerRating
    , Size
    , anyInvestmentExceeds5k
    , decoder
    , defaultInvestmentSliderSubscription
    , defaultInvestmentsPerRating
    , defaultSize
    , encode
    , encodeSize
    , investmentSizeEqual
    , investmentSlidersSubscriptions
    , investmentsPerRatingEqual
    , mkSize
    , renderInvestments
    , renderSize
    , sizeDecoder
    )

import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Data.SharedJsonStuff
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RangeSlider exposing (RangeSlider, setDimensions, setExtents, setFormatter, setStepSize, setValues)
import Types
import Util


type alias InvestmentsPerRating =
    AnyDict Int Rating Size


defaultInvestmentsPerRating : Size -> InvestmentsPerRating
defaultInvestmentsPerRating initialSize =
    Rating.initRatingDict <| List.map (\r -> ( r, initialSize )) Rating.allRatings


type alias Size =
    RangeSlider


renderSize : Size -> String
renderSize size =
    -- TODO "'Robot má investovat do úvěrů po "
    "Běžná výše investice je" ++ investmentSizeToString size ++ " Kč."


renderInvestment : ( Rating, Size ) -> String
renderInvestment ( rating, size ) =
    -- TODO "Do úvěrů s úročením " ...  "investovat po " ... " Kč."
    "S úročením " ++ Rating.showInterestPercent rating ++ " jednotlivě investovat" ++ investmentSizeToString size ++ " Kč."


renderInvestments : Size -> InvestmentsPerRating -> String
renderInvestments defaultSize_ investments =
    if Dict.Any.isEmpty investments then
        ""

    else
        Rating.ratingDictToList investments
            --filter our sizes equal to default size
            |> List.filter (\( _, invSize ) -> toIntRange invSize /= toIntRange defaultSize_)
            |> List.map renderInvestment
            |> Util.renderNonemptySection "\n- Výše investice"


investmentSizeToString : Size -> String
investmentSizeToString size =
    let
        ( mini, maxi ) =
            toIntRange size
    in
    if mini == maxi then
        " " ++ String.fromInt mini

    else if mini == 0 then
        " až " ++ String.fromInt maxi

    else
        " " ++ String.fromInt mini ++ " až " ++ String.fromInt maxi


mkSize : Int -> Int -> Size
mkSize from to =
    RangeSlider.init
        |> setStepSize (Just 200)
        |> setFormatter (\value -> String.fromFloat value ++ "Kč")
        |> setDimensions 300 57
        |> setExtents 0 20000
        |> setValues (toFloat from) (toFloat to)


defaultSize : Size
defaultSize =
    mkSize 0 200


investmentSlidersSubscriptions : InvestmentsPerRating -> Sub Types.Msg
investmentSlidersSubscriptions iprSliderStates =
    Dict.Any.toList iprSliderStates
        |> List.map (\( rtg, sliderState ) -> Sub.map (Types.ChangeInvestment rtg) (RangeSlider.subscriptions sliderState))
        |> Sub.batch


defaultInvestmentSliderSubscription : Size -> Sub Types.Msg
defaultInvestmentSliderSubscription =
    Sub.map Types.ChangeDefaultInvestment << RangeSlider.subscriptions


toIntRange : Size -> ( Int, Int )
toIntRange =
    RangeSlider.getValues >> (\( a, b ) -> ( round a, round b ))


anyInvestmentExceeds5k : Size -> InvestmentsPerRating -> Bool
anyInvestmentExceeds5k default overrides =
    Dict.Any.toList overrides
        |> List.map (\( _, size ) -> toIntRange size |> Tuple.second)
        |> List.append [ toIntRange default |> Tuple.second ]
        |> List.filter (\x -> x > 5000)
        |> (not << List.isEmpty)


investmentSizeEqual : Size -> Size -> Bool
investmentSizeEqual s1 s2 =
    RangeSlider.getValues s1 == RangeSlider.getValues s2


investmentsPerRatingEqual : InvestmentsPerRating -> InvestmentsPerRating -> Bool
investmentsPerRatingEqual ipr1 ipr2 =
    let
        getSliderValues =
            Dict.Any.values >> List.map RangeSlider.getValues
    in
    getSliderValues ipr1 == getSliderValues ipr2



-- JSON


encode : InvestmentsPerRating -> Value
encode =
    Data.SharedJsonStuff.encodeRatingToSliderDict encodeSize


decoder : Decoder InvestmentsPerRating
decoder =
    Data.SharedJsonStuff.ratingToSliderDictDecoder defaultSize sizeDecoder


encodeSize : Size -> Value
encodeSize sz =
    toIntRange sz |> (\( from, to ) -> Encode.list Encode.int [ from, to ])


sizeDecoder : Decoder Size
sizeDecoder =
    Decode.list Decode.int
        |> Decode.map
            (\xs ->
                case xs of
                    from :: to :: [] ->
                        mkSize from to

                    _ ->
                        defaultSize
            )
