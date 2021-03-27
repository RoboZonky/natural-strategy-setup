module Data.SharedJsonStuff exposing (ratingToSliderDictDecoder)

import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import RangeSlider exposing (RangeSlider)


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
