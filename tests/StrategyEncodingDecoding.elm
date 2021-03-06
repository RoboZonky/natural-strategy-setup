module StrategyEncodingDecoding exposing
    ( amountCondition
    , conditions
    , interestCondition
    , investmentsPerRating
    , portfolio
    )

import Data.Filter exposing (FilteredItem(..))
import Data.Filter.Conditions as Conditions exposing (Conditions)
import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition(..))
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition(..))
import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Data.Investment as Investment exposing (investmentsPerRatingEqual)
import Data.Portfolio as Portfolio
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Random
import Test exposing (..)
import Test.RandomStrategy as RS



-- AmountCondition


amountCondition : Test
amountCondition =
    describe "AmountCondition"
        [ fuzz amountConditionFuzzer "decode . encode == id" <|
            \cond ->
                cond
                    |> encodeAndDecode Amount.encodeCondition Amount.conditionDecoder
                    |> Expect.equal (Ok cond)
        , Test.concat <|
            List.map
                (\badInput ->
                    test ("Decoding invalid JSON " ++ badInput ++ " should fail") <|
                        \() ->
                            Decode.decodeString Amount.conditionDecoder badInput
                                |> Expect.err
                )
                [ "[]", "[1]", "[1,2,3]", "[2]", "[2,1]", "[2,1,2,3]", "[3]", "[3,1,2]", "[10,2,5]" ]
        ]


amountConditionFuzzer : Fuzzer AmountCondition
amountConditionFuzzer =
    Fuzz.oneOf
        [ Fuzz.map (AmountCondition << Amount.LessThan) Fuzz.int
        , Fuzz.map2 (\x y -> AmountCondition <| Amount.Between x y) Fuzz.int Fuzz.int
        , Fuzz.map (AmountCondition << Amount.MoreThan) Fuzz.int
        ]



-- InterestCondition


interestCondition : Test
interestCondition =
    describe "InterestCondition"
        [ fuzz interestConditionFuzzer "decode . encode == id" <|
            \cond ->
                cond
                    |> encodeAndDecode Interest.encodeCondition Interest.conditionDecoder
                    |> Expect.equal (Ok cond)
        ]


interestConditionFuzzer : Fuzzer InterestCondition
interestConditionFuzzer =
    Fuzz.map InterestCondition <|
        Fuzz.oneOf
            [ Fuzz.map Interest.LessThan ratingFuzzer
            , Fuzz.map2 (\x y -> Interest.Between x y) ratingFuzzer ratingFuzzer
            , Fuzz.map Interest.MoreThan ratingFuzzer
            ]


ratingFuzzer : Fuzzer Rating
ratingFuzzer =
    Fuzz.oneOf <| List.map Fuzz.constant Rating.allRatings



-- Conditions


hundredRandomConditions : List Conditions
hundredRandomConditions =
    Tuple.first <|
        Random.step
            (Random.list 100
                (filteredItemGen
                    |> Random.andThen (RS.conditionsGen 0)
                )
            )
            (Random.initialSeed 0)


filteredItemGen : Random.Generator FilteredItem
filteredItemGen =
    Random.uniform Loan
        [ Participation
        , Loan_And_Participation
        , Participation_To_Sell
        ]


conditions : Test
conditions =
    describe "Conditions: decode . encode == id" <|
        List.indexedMap
            (\i cond ->
                test (String.fromInt i) <|
                    \() ->
                        cond
                            |> encodeAndDecode Conditions.encodeConditions Conditions.conditionsDecoder
                            |> Expect.equal (Ok cond)
            )
            hundredRandomConditions



-- Portfolio


portfolio : Test
portfolio =
    describe "Portfolio"
        [ test "Invalid should decode with error" <|
            \() -> Decode.decodeString Portfolio.decoder "4" |> Expect.err
        , test "Valid should decode successfully" <|
            \() ->
                Decode.decodeString Portfolio.decoder "0" |> Expect.equal (Ok Portfolio.Conservative)
        ]



-- InvestmentsPerRating


investmentsPerRating : Test
investmentsPerRating =
    describe "InvestmentsPerRating"
        [ test "decode . encode == id" <|
            \() ->
                let
                    origInv =
                        Investment.defaultInvestmentsPerRating Investment.defaultSize

                    result =
                        encodeAndDecode Investment.encode Investment.decoder origInv
                in
                case result of
                    Ok decodedInv ->
                        Expect.true "InvestmentsPerRating encoding and decoding should roundtrip" (investmentsPerRatingEqual origInv decodedInv)

                    Err e ->
                        Expect.fail e
        ]



-- Utilities


encodeAndDecode : (a -> Value) -> Decoder a -> a -> Result String a
encodeAndDecode encode decoder x =
    encode x |> Encode.encode 0 |> Decode.decodeString decoder |> Result.mapError Decode.errorToString
