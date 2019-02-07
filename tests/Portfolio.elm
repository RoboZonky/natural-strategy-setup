module Portfolio exposing (stringConversions)

import Data.Portfolio exposing (allPortfolios, fromString, toString)
import Expect
import Test exposing (..)


stringConversions : Test
stringConversions =
    test "fromString . toString == id" <|
        \() ->
            List.all (\p -> fromString (toString p) == p) allPortfolios
                |> Expect.true "fromString . toString should return the same value"
