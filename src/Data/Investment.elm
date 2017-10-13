module Data.Investment
    exposing
        ( InvestmentsPerRating
        , Size
        , decoder
        , defaultInvestmentSliderSubscription
        , defaultInvestmentsPerRating
        , defaultSize
        , encode
        , encodeSize
        , investmentSlidersSubscriptions
        , renderDefaultInvestmentSize
        , renderInvestment
        , renderInvestments
        , size
        , sizeDecoder
        )

import AllDict exposing (AllDict)
import Data.Filter.Conditions.Rating as Rating exposing (Rating, ratingToString)
import Data.SharedJsonStuff
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RangeSlider exposing (RangeSlider, setDimensions, setExtents, setFormatter, setStepSize, setValues)
import Types
import Util


type alias InvestmentsPerRating =
    AllDict Rating Size Int


defaultInvestmentsPerRating : Size -> InvestmentsPerRating
defaultInvestmentsPerRating defaultSize =
    AllDict.fromList Rating.hash <| List.map (\r -> ( r, defaultSize )) Rating.allRatings


type alias Size =
    RangeSlider


renderDefaultInvestmentSize : Size -> String
renderDefaultInvestmentSize size =
    "Běžná výše investice je" ++ investmentSizeToString size ++ " Kč."


renderInvestment : ( Rating, Size ) -> String
renderInvestment ( rating, size ) =
    "Do úvěrů v ratingu " ++ ratingToString rating ++ " investovat" ++ investmentSizeToString size ++ " Kč."


renderInvestments : Size -> InvestmentsPerRating -> String
renderInvestments defaultSize investments =
    if AllDict.isEmpty investments then
        ""
    else
        AllDict.toList investments
            --filter our sizes equal to default size
            |> List.filter (\( _, invSize ) -> toIntRange invSize /= toIntRange defaultSize)
            |> List.map renderInvestment
            |> Util.renderNonemptySection "\n- Výše investice"


investmentSizeToString : Size -> String
investmentSizeToString size =
    let
        ( mini, maxi ) =
            toIntRange size
    in
    if mini == maxi then
        " " ++ toString mini
    else if mini == 0 then
        " až " ++ toString maxi
    else
        " " ++ toString mini ++ " až " ++ toString maxi


size : Int -> Int -> Size
size from to =
    RangeSlider.init
        |> setStepSize (Just 200)
        |> setFormatter (\value -> toString value ++ "Kč")
        |> setDimensions 300 57
        |> setExtents 0 5000
        |> setValues (toFloat from) (toFloat to)


defaultSize : Size
defaultSize =
    size 200 200


investmentSlidersSubscriptions : InvestmentsPerRating -> Sub Types.Msg
investmentSlidersSubscriptions iprSliderStates =
    AllDict.toList iprSliderStates
        |> List.map (\( rtg, sliderState ) -> Sub.map (Types.ChangeInvestment rtg) (RangeSlider.subscriptions sliderState))
        |> Sub.batch


defaultInvestmentSliderSubscription : Size -> Sub Types.Msg
defaultInvestmentSliderSubscription =
    Sub.map Types.ChangeDefaultInvestment << RangeSlider.subscriptions


toIntRange : Size -> ( Int, Int )
toIntRange =
    RangeSlider.getValues >> (\( a, b ) -> ( round a, round b ))



-- JSON


encode : InvestmentsPerRating -> Value
encode =
    Data.SharedJsonStuff.encodeRatingToSliderDict encodeSize


decoder : Decoder InvestmentsPerRating
decoder =
    Data.SharedJsonStuff.ratingToSliderDictDecodr sizeDecoder


encodeSize : Size -> Value
encodeSize sz =
    toIntRange sz |> (\( from, to ) -> Encode.list [ Encode.int from, Encode.int to ])


sizeDecoder : Decoder Size
sizeDecoder =
    Decode.list Decode.int
        |> Decode.map
            (\xs ->
                case xs of
                    from :: to :: [] ->
                        size from to

                    _ ->
                        size 200 200
            )
