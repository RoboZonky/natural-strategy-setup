module Util exposing
    ( and
    , emptyToZero
    , enumDecoder
    , enumEncoder
    , formatPercentage
    , intListToString
    , joinNonemptyLines
    , orList
    , parseFloat
    , parseInt
    , renderNonemptySection
    , stringListToString
    , viewErrors
    , zeroToEmpty
    , zeroToEmptyFloat
    )

import FormatNumber
import FormatNumber.Locales exposing (Locale)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra


orList : (a -> String) -> List a -> String
orList itemToString list =
    case list of
        [] ->
            "VYBERTE ASPOŇ JEDNU HODNOTU"

        r :: [] ->
            itemToString r

        r1 :: r2 :: [] ->
            itemToString r1 ++ " nebo " ++ itemToString r2

        r1 :: rest ->
            itemToString r1 ++ ", " ++ orList itemToString rest


joinNonemptyLines : List String -> String
joinNonemptyLines =
    String.join "\n" << List.filter (not << String.isEmpty)


renderNonemptySection : String -> List String -> String
renderNonemptySection sectionTitle list =
    case list of
        [] ->
            ""

        nonempty ->
            joinNonemptyLines <| sectionTitle :: nonempty



-- Used for entering numeric inputs. We want to represent 0 as empty input value (so that user can delete everything from the input)
-- and conversely we want to treat "" input value to be parsed as 0


zeroToEmpty : Int -> String
zeroToEmpty x =
    if x == 0 then
        ""

    else
        String.fromInt x


zeroToEmptyFloat : Float -> String
zeroToEmptyFloat x =
    if x == 0 then
        ""

    else
        String.fromFloat x


emptyToZero : String -> String
emptyToZero s =
    if String.isEmpty s then
        "0"

    else
        s


parseInt : String -> Maybe Int
parseInt =
    String.toInt << emptyToZero


parseFloat : String -> Maybe Float
parseFloat =
    String.toFloat << emptyToZero


and : List Bool -> Bool
and =
    List.all Basics.identity


viewErrors : List String -> Html a
viewErrors errors =
    if List.isEmpty errors then
        text ""

    else
        div [ style "color" "red" ] [ text <| String.join ";" errors ]


intListToString : List Int -> String
intListToString =
    stringListToString << List.map String.fromInt


stringListToString : List String -> String
stringListToString xs =
    "[" ++ String.join "," xs ++ "]"


formatPercentage : Float -> String
formatPercentage =
    FormatNumber.format czechLocale


czechLocale : Locale
czechLocale =
    { decimals = 2
    , thousandSeparator = ""
    , decimalSeparator = ","
    , negativePrefix = "−"
    , negativeSuffix = ""
    , positivePrefix = ""
    , positiveSuffix = ""
    , zeroPrefix = ""
    , zeroSuffix = ""
    }



-- JSON


enumEncoder : List a -> a -> Value
enumEncoder allValues val =
    List.Extra.elemIndex val allValues
        |> (\maybeIndex ->
                case maybeIndex of
                    Just index ->
                        Encode.int index

                    Nothing ->
                        -- Prior to elm 0.19 this used to be crash.
                        -- But I still need some way to be able to detect failure condition
                        Encode.int -1
           )


enumDecoder : String -> List a -> Decoder a
enumDecoder nameOfDecodedThing allValues =
    Decode.andThen
        (\index ->
            List.Extra.getAt index allValues
                |> (\maybeVal ->
                        case maybeVal of
                            Just val ->
                                Decode.succeed val

                            Nothing ->
                                Decode.fail <|
                                    "Invalid index "
                                        ++ String.fromInt index
                                        ++ " when trying to decode "
                                        ++ nameOfDecodedThing
                   )
        )
        Decode.int
