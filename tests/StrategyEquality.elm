module StrategyEquality exposing (..)

import Data.Filter as Filters
import Data.Strategy as Strategy exposing (defaultStrategyConfiguration)
import Expect
import Test exposing (Test, describe, test)


strategyEquality : Test
strategyEquality =
    describe "Strategy.strategyEqual"
        [ test "default strategy" <|
            \() ->
                Strategy.strategyEqual defaultStrategyConfiguration defaultStrategyConfiguration
                    |> Expect.true "defaultStrategyConfiguration should be equal to itself"
        , test "slightly modified" <|
            \() ->
                let
                    modifiedStrategy =
                        { defaultStrategyConfiguration | buyingConfig = Filters.InvestNothing }
                in
                Strategy.strategyEqual
                    defaultStrategyConfiguration
                    modifiedStrategy
                    |> Expect.false "different strategy configs should not be equal"
        ]
