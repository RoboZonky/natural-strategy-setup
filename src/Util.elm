module Util
    exposing
        ( and
        , emptyToZero
        , enumDecoder
        , enumEncoder
        , joinNonemptyLines
        , orList
        , renderNonemptySection
        , validate
        , viewErrors
        , zeroToEmpty
        )

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


zeroToEmpty : number -> String
zeroToEmpty x =
    if x == 0 then
        ""
    else
        toString x


emptyToZero : String -> String
emptyToZero s =
    if String.isEmpty s then
        "0"
    else
        s


validate : Bool -> String -> List String
validate errorCondition error =
    if errorCondition then
        [ error ]
    else
        []


and : List Bool -> Bool
and =
    List.all Basics.identity


viewErrors : List String -> Html a
viewErrors errors =
    if List.isEmpty errors then
        text ""
    else
        div [ style [ ( "color", "red" ) ] ] [ text <| String.join ";" errors ]



-- JSON


enumEncoder : List a -> a -> Value
enumEncoder allValues val =
    List.Extra.elemIndex val allValues
        |> (\maybeIndex ->
                case maybeIndex of
                    Just index ->
                        Encode.int index

                    Nothing ->
                        Debug.crash <|
                            "Impossible happended - value "
                                ++ toString val
                                ++ " was not in the list of all values "
                                ++ toString allValues
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
                                        ++ toString index
                                        ++ " when trying to decode "
                                        ++ nameOfDecodedThing
                   )
        )
        Decode.int
