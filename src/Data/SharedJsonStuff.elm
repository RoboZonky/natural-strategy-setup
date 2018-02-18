module Data.SharedJsonStuff exposing (encodeRatingToSliderDict, ratingToSliderDictDecodr)

import AllDict exposing (AllDict)
import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RangeSlider exposing (RangeSlider)


{-| Encoders / Decoders shared by InvestmentsPerRating and PortfolioShares
-}
encodeRatingToSliderDict : (RangeSlider -> Value) -> AllDict Rating RangeSlider Int -> Value
encodeRatingToSliderDict sliderEncoder =
    AllDict.toList
        >> List.map (\( _ {- assuming that rating is always sorted in order or Rating declaration, so just encoding slider states -}, slider ) -> sliderEncoder slider)
        >> Encode.list


ratingToSliderDictDecodr : Decoder RangeSlider -> Decoder (AllDict Rating RangeSlider Int)
ratingToSliderDictDecodr sliderStateDecoder =
    Decode.list sliderStateDecoder
        |> Decode.map
            (\sliderStates ->
                {- Takind advantage that encoded slider states are ordered from A** down to D -}
                List.map2 (,) Rating.allRatings sliderStates
                    |> AllDict.fromList Rating.hash
            )
