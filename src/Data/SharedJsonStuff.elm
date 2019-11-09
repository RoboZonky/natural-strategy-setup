module Data.SharedJsonStuff exposing (encodeRatingToSliderDict, ratingToSliderDictDecoder)

import Data.Filter.Conditions.Rating as Rating exposing (Rating(..), ratingDictToList)
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RangeSlider exposing (RangeSlider)


{-| Encoders / Decoders shared by InvestmentsPerRating and PortfolioStructure
-}
encodeRatingToSliderDict : (RangeSlider -> Value) -> AnyDict Int Rating RangeSlider -> Value
encodeRatingToSliderDict sliderEncoder dict =
    ratingDictToList dict
        |> Encode.list
            (\( _ {- assuming that rating is always sorted in order of rating's toInterestPercent, so just encoding slider states -}, slider ) ->
                sliderEncoder slider
            )


ratingToSliderDictDecoder : RangeSlider -> Decoder RangeSlider -> Decoder (AnyDict Int Rating RangeSlider)
ratingToSliderDictDecoder defaultSliderState sliderStateDecoder =
    Decode.list sliderStateDecoder
        |> Decode.andThen
            (\sliderStates ->
                case sliderStates of
                    [ _, _, _, _, _, _, _, _, _, _, _ {- 11 values since RZ 5.1.1 -} ] ->
                        -- Taking advantage that encoded slider states are ordered based on toInterestPercent
                        List.map2 Tuple.pair Rating.allRatings sliderStates
                            |> Rating.initRatingDict
                            |> Decode.succeed

                    [ aaaaa, aaaa, aaa, aae, aa, ae, a, b, c, d {- 10 values in RZ 5.1.0 -} ] ->
                        -- Backward compatibility with RZ 5.1.0 strategies
                        List.map2 Tuple.pair Rating.allRatings [ defaultSliderState, aaaaa, aaaa, aaa, aae, aa, ae, a, b, c, d ]
                            |> Rating.initRatingDict
                            |> Decode.succeed

                    [ aaaaa, aaaa, aaa, aa, a, b, c, d {- 8 values for RZ older than 5.1.0 -} ] ->
                        -- Backward compatibility with pre RZ 5.1.0 strategies
                        List.map2 Tuple.pair Rating.allRatings [ defaultSliderState, aaaaa, aaaa, aaa, defaultSliderState, aa, defaultSliderState, a, b, c, d ]
                            |> Rating.initRatingDict
                            |> Decode.succeed

                    _ ->
                        "Unexpected number of ratings when decoding range sliders: "
                            ++ String.fromInt (List.length sliderStates)
                            |> Decode.fail
            )
