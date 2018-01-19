module ExitConfig exposing (exitConfigValidation)

import Data.ExitConfig as ExitConfig exposing (..)
import Expect
import Test exposing (..)
import Time.Date exposing (Date)
import Time.DateTime as DateTime


{-| "today" used for validation of dates and times
-}
dummyToday : Date
dummyToday =
    DateTime.date DateTime.epoch


exitConfigValidation : Test
exitConfigValidation =
    describe "Exit config"
        [ describe "validate - should return empty list of validation errors vor valid configuration"
            [ test "DontExit" <|
                \() -> ExitConfig.validate dummyToday DontExit |> Expect.equal []
            , test "ExitBy" <|
                \() -> ExitConfig.validate dummyToday (ExitBy "31.12.2018") |> Expect.equal []
            , test "ExitByWithSelloff" <|
                \() -> ExitConfig.validate dummyToday (ExitByWithSelloff "31.12.2018" "1.1.2018") |> Expect.equal []
            ]
        , describe
            "validate - should return list of validation errors for invalid dates"
            [ test "ExitBy - nonsense" <|
                \() ->
                    ExitConfig.validate dummyToday (ExitBy "nonsense")
                        |> Expect.equal [ "Datum opuštění musí mít formát den.měsíc.rok" ]
            , test "ExitBy - two dots, but not numbers" <|
                \() ->
                    ExitConfig.validate dummyToday (ExitBy "not.3.numbers")
                        |> Expect.equal [ "Datum opuštění - den, měsíc i rok musí být čísla" ]
            , test "ExitBy - three numbers, but invalid date" <|
                \() ->
                    ExitConfig.validate dummyToday (ExitBy "32.13.2017")
                        |> Expect.equal [ "Datum opuštění 32.13.2017 neexistuje v kalendáři" ]
            , test "ExitBy - past date" <|
                \() ->
                    ExitConfig.validate dummyToday (ExitBy "1.1.1900")
                        |> Expect.equal [ "Datum opuštění musí být v budoucnosti" ]
            , test "ExitByWithSelloff - past exit date" <|
                \() ->
                    ExitConfig.validate dummyToday (ExitByWithSelloff "1.1.1900" "1.1.2100")
                        |> Expect.equal
                            [ "Datum zahájení výprodeje musí být před datem opuštění" ]
            , test "ExitByWithSelloff - past selloff date" <|
                \() ->
                    ExitConfig.validate dummyToday (ExitByWithSelloff "1.1.2000" "1.1.1900")
                        |> Expect.equal
                            [ "Datum zahájení výprodeje musí být v budoucnosti" ]
            , test "ExitByWithSelloff - both invalid" <|
                \() ->
                    ExitConfig.validate dummyToday (ExitByWithSelloff "nonsense" "100.200.300")
                        |> Expect.equal
                            [ "Datum opuštění musí mít formát den.měsíc.rok"
                            , "Datum zahájení výprodeje 100.200.300 neexistuje v kalendáři"
                            ]
            ]
        ]
