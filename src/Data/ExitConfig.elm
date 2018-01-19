module Data.ExitConfig
    exposing
        ( ExitConfig(..)
        , decoder
        , encode
        , render
        , validate
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import String exposing (toInt)
import Time.Date as Date exposing (Date)


type ExitConfig
    = DontExit
    | ExitBy String
    | ExitByWithSelloff String String


{-| Parse date from string of the form "d.M.YYYY"
-}
parseDateString : String -> Result String Date
parseDateString input =
    case String.split "." input of
        [ dayStr, monthStr, yearStr ] ->
            case Result.map3 (,,) (toInt dayStr) (toInt monthStr) (toInt yearStr) of
                Ok ( day, month, year ) ->
                    if Date.isValidDate year month day then
                        Ok <| Date.fromTuple ( year, month, day )
                    else
                        Err <| dayStr ++ "." ++ monthStr ++ "." ++ yearStr ++ " neexistuje v kalendáři"

                Err _ ->
                    Err "- den, měsíc i rok musí být čísla"

        _ ->
            Err "musí mít formát den.měsíc.rok"


render : ExitConfig -> String
render exitConfig =
    case exitConfig of
        DontExit ->
            ""

        ExitBy exitDateStr ->
            "Opustit Zonky k " ++ exitDateStr ++ "."

        ExitByWithSelloff exitDateStr selloffDateStr ->
            "Opustit Zonky k " ++ exitDateStr ++ ", výprodej zahájit " ++ selloffDateStr ++ "."


validate : Date -> ExitConfig -> List String
validate today exitConfig =
    case exitConfig of
        DontExit ->
            []

        ExitBy exitDateStr ->
            case parseDateString exitDateStr of
                Ok exitDate ->
                    if isFirstBeforeSecond exitDate today then
                        [ "Datum opuštění musí být v budoucnosti" ]
                    else
                        []

                Err _ ->
                    validateDateString "Datum opuštění " exitDateStr

        ExitByWithSelloff exitDateStr selloffDateStr ->
            case Result.map2 (,) (parseDateString exitDateStr) (parseDateString selloffDateStr) of
                Ok ( exitDate, selloffDate ) ->
                    if isFirstBeforeSecond selloffDate today then
                        [ "Datum zahájení výprodeje musí být v budoucnosti" ]
                    else if isFirstBeforeSecond exitDate selloffDate then
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


isFirstBeforeSecond : Date -> Date -> Bool
isFirstBeforeSecond date1 date2 =
    Date.compare date1 date2 == LT



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
                        Decode.fail <| "Unable to decode ExitConfig from " ++ toString listOfStrings
            )


encode : ExitConfig -> Value
encode exitConfig =
    case exitConfig of
        DontExit ->
            Encode.list [ Encode.string "0" ]

        ExitBy date ->
            Encode.list [ Encode.string "1", Encode.string date ]

        ExitByWithSelloff date1 date2 ->
            Encode.list [ Encode.string "2", Encode.string date1, Encode.string date2 ]
