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
        >> List.map (\( rtg, slider ) -> ( toString rtg, sliderEncoder slider ))
        >> Encode.object


ratingToSliderDictDecodr : Decoder RangeSlider -> Decoder (AllDict Rating RangeSlider Int)
ratingToSliderDictDecodr sliderStateDecoder =
    Decode.keyValuePairs sliderStateDecoder
        |> Decode.map
            (List.filterMap
                (\( rtgStr, size ) ->
                    case Rating.ratingFromString rtgStr of
                        Just rtg ->
                            Just ( rtg, size )

                        Nothing ->
                            Nothing
                )
                -- TODO this might be dangerous, because some keys could be duplicated / missing. Figure out how to validate incoming JSON to prevent invalid IPRs
                >> AllDict.fromList Rating.hash
            )
