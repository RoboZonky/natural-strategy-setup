module ExitConfig exposing (exitConfigValidation)

import Data.ExitConfig as ExitConfig exposing (..)
import Expect
import Test exposing (..)


exitConfigValidation : Test
exitConfigValidation =
    describe "Exit config"
        [ describe "validate - should return empty list of validation errors vor valid configuration"
            [ test "DontExit" <|
                \() -> ExitConfig.validate DontExit |> Expect.equal []
            , test "ExitBy" <|
                \() -> ExitConfig.validate (ExitBy "31.12.2018") |> Expect.equal []
            , test "ExitByWithSelloff" <|
                \() -> ExitConfig.validate (ExitByWithSelloff "31.12.2018" "1.1.2018") |> Expect.equal []
            ]
        , describe
            "validate - should return list of validation errors for invalid dates"
            [ test "ExitBy - nonsense" <|
                \() ->
                    ExitConfig.validate (ExitBy "nonsense")
                        |> Expect.equal [ "Datum opuštění musí mít formát den.měsíc.rok" ]
            , test "ExitBy - two dots, but not numbers" <|
                \() ->
                    ExitConfig.validate (ExitBy "not.3.numbers")
                        |> Expect.equal [ "Datum opuštění - den, měsíc i rok musí být čísla" ]
            , test "ExitBy - three numbers, but invalid date" <|
                \() ->
                    ExitConfig.validate (ExitBy "32.13.2017")
                        |> Expect.equal [ "Datum opuštění 32.13.2017 neexistuje v kalendáři" ]
            , test "ExitByWithSelloff - past exit date" <|
                \() ->
                    ExitConfig.validate (ExitByWithSelloff "1.1.1900" "1.1.2100")
                        |> Expect.equal
                            [ "Datum zahájení výprodeje musí být před datem opuštění" ]
            , test "ExitByWithSelloff - both invalid" <|
                \() ->
                    ExitConfig.validate (ExitByWithSelloff "nonsense" "100.200.300")
                        |> Expect.equal
                            [ "Datum opuštění musí mít formát den.měsíc.rok"
                            , "Datum zahájení výprodeje 100.200.300 neexistuje v kalendáři"
                            ]
            ]
        ]
