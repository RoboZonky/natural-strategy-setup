module Data.Migration.Strategy.V4.PortfolioStructure exposing
    ( PortfolioShares
    , decoder
    , decoderFromPortfolio
    , toIntRange
    )

import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Data.Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as PortfolioStructure
import Data.SharedJsonStuff
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import RangeSlider exposing (RangeSlider, setDimensions, setExtents, setFormatter, setStepSize, setValues)


type alias PortfolioShares =
    AnyDict Int Rating Share


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



-- JSON


decoder : Decoder PortfolioShares
decoder =
    Data.SharedJsonStuff.ratingToSliderDictDecoder defaultShare shareDecoder


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
