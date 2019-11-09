module Data.Migration.Strategy.V4.PortfolioStructure exposing
    ( PortfolioShare
    , PortfolioShares
    , balanced
    , conservative
    , decoder
    , decoderFromPortfolio
    , encode
    , percentageShare
    , progressive
    , toIntRange
    , validate
    )

import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as PortfolioStructure
import Data.SharedJsonStuff
import Data.Validate as Validate
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RangeSlider exposing (RangeSlider, setDimensions, setExtents, setFormatter, setStepSize, setValues)


type alias PortfolioShares =
    AnyDict Int Rating Share


type alias PortfolioShare =
    ( Rating, Share )


type alias Share =
    RangeSlider


toIntRange : Share -> ( Int, Int )
toIntRange =
    RangeSlider.getValues >> (\( a, b ) -> ( round a, round b ))


percentageShare : Float -> Float -> Share
percentageShare from to =
    RangeSlider.init
        |> setStepSize (Just 1.0)
        |> setFormatter (\value -> String.fromFloat value ++ "%")
        |> setDimensions 300 57
        |> setExtents 0 100
        |> setValues from to


validate : PortfolioShares -> List String
validate portfolioShares =
    let
        sumOfShareMinimums =
            Dict.Any.foldr (\_ sliderState sumAcc -> sumAcc + getSliderMinimum sliderState) 0 portfolioShares
                |> round
    in
    Validate.validate (sumOfShareMinimums /= 100) <|
        "Součet minim musí být přesně 100% (teď je "
            ++ String.fromInt sumOfShareMinimums
            ++ "%)"


getSliderMinimum : RangeSlider -> Float
getSliderMinimum =
    Tuple.first << RangeSlider.getValues



-- JSON


encode : PortfolioShares -> Value
encode =
    Data.SharedJsonStuff.encodeRatingToSliderDict encodeShare


decoder : Decoder PortfolioShares
decoder =
    Data.SharedJsonStuff.ratingToSliderDictDecoder defaultShare shareDecoder


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
                        percentageShare (toFloat from) (toFloat to)

                    _ ->
                        defaultShare
            )


defaultShare : Share
defaultShare =
    percentageShare 0 0


decoderFromPortfolio : Portfolio -> Decoder PortfolioShares
decoderFromPortfolio portfolio =
    case portfolio of
        Conservative ->
            Decode.succeed conservative

        Balanced ->
            Decode.succeed balanced

        Progressive ->
            Decode.succeed progressive

        UserDefined ->
            Decode.field "i" decoder


conservative : PortfolioShares
conservative =
    initShares PortfolioStructure.conservativeShares


balanced : PortfolioShares
balanced =
    initShares PortfolioStructure.balancedShares


progressive : PortfolioShares
progressive =
    initShares PortfolioStructure.progressiveShares


initShares : List ( Rating, Float ) -> PortfolioShares
initShares =
    List.map (\( rtg, x ) -> ( rtg, percentageShare x x ))
        >> Rating.initRatingDict
