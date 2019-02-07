module Data.SharedJsonStuff exposing (encodeRatingToSliderDict, ratingToSliderDictDecodr)

import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RangeSlider exposing (RangeSlider)


{-| Encoders / Decoders shared by InvestmentsPerRating and PortfolioShares
-}
encodeRatingToSliderDict : (RangeSlider -> Value) -> AnyDict Int Rating RangeSlider -> Value
encodeRatingToSliderDict sliderEncoder dict =
    Dict.Any.toList dict
        |> Encode.list
            (\( _ {- assuming that rating is always sorted in order or Rating declaration, so just encoding slider states -}, slider ) ->
                sliderEncoder slider
            )


ratingToSliderDictDecodr : Decoder RangeSlider -> Decoder (AnyDict Int Rating RangeSlider)
ratingToSliderDictDecodr sliderStateDecoder =
    Decode.list sliderStateDecoder
        |> Decode.map
            (\sliderStates ->
                {- Takind advantage that encoded slider states are ordered from A** down to D -}
                List.map2 (\a b -> ( a, b )) Rating.allRatings sliderStates
                    |> Rating.initRatingDict
            )
