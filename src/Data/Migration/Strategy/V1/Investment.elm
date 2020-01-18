module Data.Migration.Strategy.V1.Investment exposing
    ( InvestmentsPerRating
    , Size
    , decoder
    , defaultSize
    , mkSize
    , sizeDecoder
    )

import Data.Filter.Conditions.Rating exposing (Rating)
import Data.SharedJsonStuff
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import RangeSlider exposing (RangeSlider, setDimensions, setExtents, setFormatter, setStepSize, setValues)


type alias InvestmentsPerRating =
    AnyDict Int Rating Size


type alias Size =
    RangeSlider


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


decoder : Decoder InvestmentsPerRating
decoder =
    Data.SharedJsonStuff.ratingToSliderDictDecoder defaultSize sizeDecoder


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
