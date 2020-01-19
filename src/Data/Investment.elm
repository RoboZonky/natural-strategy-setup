module Data.Investment exposing
    ( InvestmentsPerRating
    , Msg(..)
    , Size
    , anyInvestmentExceeds5k
    , decoder
    , defaultInvestmentsPerRating
    , defaultSize
    , encode
    , encodeSize
    , fromInt
    , investmentSizeEqual
    , investmentsPerRatingEqual
    , renderInvestments
    , renderSize
    , sizeDecoder
    , toCzkString
    , toInt
    , update
    )

import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type alias InvestmentsPerRating =
    AnyDict Int Rating Size


defaultInvestmentsPerRating : Size -> InvestmentsPerRating
defaultInvestmentsPerRating initialSize =
    Rating.initRatingDict <| List.map (\r -> ( r, initialSize )) Rating.allRatings


type Size
    = Size Int


type Msg
    = SetValue Int


update : Msg -> Size -> Size
update msg (Size _) =
    case msg of
        SetValue newValue ->
            Size newValue


fromInt : Int -> Size
fromInt =
    Size


toInt : Size -> Int
toInt (Size sz) =
    sz


toCzkString : Size -> String
toCzkString pis =
    investmentSizeToString pis ++ " Kč"


renderSize : Size -> String
renderSize pis =
    "Robot má investovat do úvěrů po " ++ investmentSizeToString pis ++ " Kč."


renderInvestment : ( Rating, Size ) -> String
renderInvestment ( rating, size ) =
    "Do úvěrů s úročením " ++ Rating.showInterestPercent rating ++ " investovat po " ++ investmentSizeToString size ++ " Kč."


renderInvestments : Size -> InvestmentsPerRating -> String
renderInvestments defaultSize_ investments =
    if Dict.Any.isEmpty investments then
        ""

    else
        Rating.ratingDictToList investments
            --filter our sizes equal to default size
            |> List.filter (\( _, invSize ) -> invSize /= defaultSize_)
            |> List.map renderInvestment
            |> Util.renderNonemptySection "\n- Výše investice"



-- TODO maybe remove this


investmentSizeToString : Size -> String
investmentSizeToString (Size s) =
    String.fromInt s


defaultSize : Size
defaultSize =
    Size 200


anyInvestmentExceeds5k : Size -> InvestmentsPerRating -> Bool
anyInvestmentExceeds5k default overrides =
    default
        :: Dict.Any.values overrides
        |> List.map toInt
        |> List.filter (\x -> x > 5000)
        |> (not << List.isEmpty)



-- TODO remove


investmentSizeEqual : Size -> Size -> Bool
investmentSizeEqual =
    (==)


investmentsPerRatingEqual : InvestmentsPerRating -> InvestmentsPerRating -> Bool
investmentsPerRatingEqual ipr1 ipr2 =
    Dict.Any.values ipr1 == Dict.Any.values ipr2



-- JSON


encode : InvestmentsPerRating -> Value
encode =
    Rating.ratingDictToList
        >> Encode.list
            (\( _ {- assuming that rating is always sorted in order of rating's toInterestPercent, so just encoding slider states -}, size ) ->
                encodeSize size
            )


decoder : Decoder InvestmentsPerRating
decoder =
    Decode.list sizeDecoder
        |> Decode.andThen
            (\sizes ->
                case sizes of
                    [ _, _, _, _, _, _, _, _, _, _, _ {- 11 values since RZ 5.1.1 -} ] ->
                        List.map2 Tuple.pair Rating.allRatings sizes
                            |> Rating.initRatingDict
                            |> Decode.succeed

                    _ ->
                        "Unexpected number of values when decoding InvestmentPerRating: "
                            ++ String.fromInt (List.length sizes)
                            |> Decode.fail
            )


encodeSize : Size -> Value
encodeSize (Size sz) =
    Encode.int sz


sizeDecoder : Decoder Size
sizeDecoder =
    Decode.map Size Decode.int
