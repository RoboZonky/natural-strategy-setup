module Data.ExitConfig exposing
    ( ExitConfig(..)
    , decoder
    , encode
    , parseDateString
    , render
    , validate
    )

import Data.DateValidation exposing (isValidDate)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import String exposing (toInt)
import Time exposing (Month(..), Posix)
import Time.Extra as TE
import Util


type ExitConfig
    = DontExit
    | ExitBy String
    | ExitByWithSelloff String String


{-| Parse date from string of the form "d.M.YYYY"
-}
parseDateString : String -> Result String Posix
parseDateString input =
    case List.map toInt (String.split "." input) of
        [ mayDay, mayMonth, mayYear ] ->
            Maybe.map3 posixFromDayMonthYear mayDay mayMonth mayYear
                |> Maybe.withDefault (Err "- den, měsíc i rok musí být čísla")

        _ ->
            Err "musí mít formát den.měsíc.rok"


posixFromDayMonthYear : Int -> Int -> Int -> Result String Posix
posixFromDayMonthYear day monthNum year =
    let
        error =
            String.fromInt day ++ "." ++ String.fromInt monthNum ++ "." ++ String.fromInt year ++ " neexistuje v kalendáři"
    in
    if isValidDate year monthNum day then
        case intToMonth monthNum of
            Just month ->
                Ok <| toPosix day month year

            Nothing ->
                Err error

    else
        Err error


toPosix : Int -> Month -> Int -> Posix
toPosix day month year =
    TE.partsToPosix Time.utc
        { year = year
        , month = month
        , day = day
        , hour = 0
        , minute = 0
        , second = 0
        , millisecond = 0
        }


intToMonth : Int -> Maybe Month
intToMonth monthNum =
    case monthNum of
        1 ->
            Just Jan

        2 ->
            Just Feb

        3 ->
            Just Mar

        4 ->
            Just Apr

        5 ->
            Just May

        6 ->
            Just Jun

        7 ->
            Just Jul

        8 ->
            Just Aug

        9 ->
            Just Sep

        10 ->
            Just Oct

        11 ->
            Just Nov

        12 ->
            Just Dec

        _ ->
            Nothing


render : ExitConfig -> String
render exitConfig =
    case exitConfig of
        DontExit ->
            ""

        ExitBy exitDateStr ->
            "Opustit Zonky k " ++ exitDateStr ++ "."

        ExitByWithSelloff exitDateStr selloffDateStr ->
            "Opustit Zonky k " ++ exitDateStr ++ ", výprodej zahájit " ++ selloffDateStr ++ "."


validate : ExitConfig -> List String
validate exitConfig =
    case exitConfig of
        DontExit ->
            []

        ExitBy exitDateStr ->
            validateDateString "Datum opuštění " exitDateStr

        ExitByWithSelloff exitDateStr selloffDateStr ->
            case Result.map2 (\a b -> ( a, b )) (parseDateString exitDateStr) (parseDateString selloffDateStr) of
                Ok ( exitDate, selloffDate ) ->
                    if isFirstBeforeSecond exitDate selloffDate then
                        [ "Datum zahájení výprodeje musí být před datem opuštění" ]

                    else
                        []

                Err _ ->
                    validateDateString "Datum opuštění " exitDateStr
                        ++ validateDateString "Datum zahájení výprodeje " selloffDateStr


validateDateString : String -> String -> List String
validateDateString errorPrefix dateString =
    case parseDateString dateString of
        Err err ->
            [ errorPrefix ++ err ]

        Ok _ ->
            []


isFirstBeforeSecond : Posix -> Posix -> Bool
isFirstBeforeSecond date1 date2 =
    Time.posixToMillis date1 < Time.posixToMillis date2



-- JSON


decoder : Decoder ExitConfig
decoder =
    Decode.list Decode.string
        |> Decode.andThen
            (\listOfStrings ->
                case listOfStrings of
                    [ "0" ] ->
                        Decode.succeed DontExit

                    [ "1", date ] ->
                        Decode.succeed <| ExitBy date

                    [ "2", date1, date2 ] ->
                        Decode.succeed <| ExitByWithSelloff date1 date2

                    _ ->
                        Decode.fail <| "Unable to decode ExitConfig from " ++ Util.stringListToString listOfStrings
            )


encode : ExitConfig -> Value
encode exitConfig =
    case exitConfig of
        DontExit ->
            Encode.list Encode.string [ "0" ]

        ExitBy date ->
            Encode.list Encode.string [ "1", date ]

        ExitByWithSelloff date1 date2 ->
            Encode.list Encode.string [ "2", date1, date2 ]
