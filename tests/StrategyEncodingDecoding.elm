module StrategyEncodingDecoding exposing (..)

import AllDict
import Data.Filter exposing (FilteredItem(Loan))
import Data.Filter.Conditions as Conditions exposing (Conditions)
import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition(AmountCondition))
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition(InterestCondition))
import Data.Investment as Investment
import Data.Portfolio as Portfolio
import Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Random
import RangeSlider
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
    Fuzz.oneOf
        [ Fuzz.map (InterestCondition << Interest.LessThan) Fuzz.float
        , Fuzz.map2 (\x y -> InterestCondition <| Interest.Between x y) Fuzz.float Fuzz.float
        , Fuzz.map (InterestCondition << Interest.MoreThan) Fuzz.float
        ]



-- Conditions


hundredRandomConditions : List Conditions
hundredRandomConditions =
    Tuple.first <|
        Random.step
            (Random.list 100 (RS.conditionsGen 0 Loan))
            (Random.initialSeed 0)


conditions : Test
conditions =
    describe "Conditions: decode . encode == id" <|
        List.indexedMap
            (\i cond ->
                test (toString i) <|
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
            \() -> Decode.decodeString Portfolio.decoder "bla" |> Expect.err
        , test "Valid should decode successfully" <|
            \() ->
                Decode.decodeString Portfolio.decoder "\"Conservative\"" |> Expect.equal (Ok Portfolio.Conservative)
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
                in
                encodeAndDecode Investment.encode Investment.decoder origInv
                    |> Result.map makeIprComparable
                    |> Expect.equal (Ok <| makeIprComparable origInv)
        ]


{-| IPR Contains RangeSlider.model which contains functions, which can't be compared for equality
-}
makeIprComparable : Investment.InvestmentsPerRating -> Dict String ( Float, Float )
makeIprComparable ipr =
    AllDict.toList ipr |> List.map (\( rtg, slider ) -> ( toString rtg, RangeSlider.getValues slider )) |> Dict.fromList



-- Utilities


encodeAndDecode : (a -> Value) -> Decoder a -> a -> Result String a
encodeAndDecode encode decoder x =
    encode x |> Encode.encode 0 |> Decode.decodeString decoder
