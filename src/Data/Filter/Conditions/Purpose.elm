module Data.Filter.Conditions.Purpose
    exposing
        ( Purpose(..)
        , PurposeCondition(..)
        , PurposeMsg
        , allPurposes
        , conditionDecoder
        , defaultCondition
        , encodeCondition
        , form
        , renderCondition
        , update
        , validationErrors
        )

import Bootstrap.Form.Checkbox as Checkbox
import Html exposing (Html, div)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type Purpose
    = AUTO_MOTO
    | CESTOVANI
    | DOMACNOST
    | ELEKTRONIKA
    | REFINANCOVANI_PUJCEK
    | VLASTNI_PROJEKT
    | VZDELANI
    | ZDRAVI
    | JINE


allPurposes : List Purpose
allPurposes =
    [ AUTO_MOTO, CESTOVANI, DOMACNOST, ELEKTRONIKA, REFINANCOVANI_PUJCEK, VLASTNI_PROJEKT, VZDELANI, ZDRAVI, JINE ]


type PurposeCondition
    = PurposeList (List Purpose)


defaultCondition : PurposeCondition
defaultCondition =
    PurposeList []


purposeToString : Purpose -> String
purposeToString purpose =
    case purpose of
        AUTO_MOTO ->
            "auto-moto"

        CESTOVANI ->
            "cestování"

        DOMACNOST ->
            "domácnost"

        ELEKTRONIKA ->
            "elektronika"

        REFINANCOVANI_PUJCEK ->
            "refinancování půjček"

        VLASTNI_PROJEKT ->
            "vlastní projekt"

        VZDELANI ->
            "vzdělání"

        ZDRAVI ->
            "zdraví"

        JINE ->
            "jiné"


renderCondition : PurposeCondition -> String
renderCondition (PurposeList list) =
    "účel je " ++ renderPurposeList list


renderPurposeList : List Purpose -> String
renderPurposeList =
    Util.orList purposeToString


validationErrors : PurposeCondition -> List String
validationErrors (PurposeList rlist) =
    Util.validate (List.isEmpty rlist) "Účel úvěru: zvolte aspoň jeden"


type PurposeMsg
    = AddPurpose Purpose
    | RemovePurpose Purpose


update : PurposeMsg -> PurposeCondition -> PurposeCondition
update msg (PurposeList plist) =
    case msg of
        AddPurpose p ->
            PurposeList (p :: plist)

        RemovePurpose p ->
            PurposeList (List.filter (\pu -> pu /= p) plist)


form : PurposeCondition -> Html PurposeMsg
form (PurposeList plist) =
    allPurposes
        |> List.map (\p -> purposeCheckbox p (List.member p plist))
        |> div []


purposeCheckbox : Purpose -> Bool -> Html PurposeMsg
purposeCheckbox purpose isEnabled =
    Checkbox.checkbox
        [ Checkbox.id ("purpose_" ++ toString purpose)
        , Checkbox.onCheck
            (\checked ->
                if checked then
                    AddPurpose purpose
                else
                    RemovePurpose purpose
            )
        , Checkbox.checked isEnabled
        , Checkbox.inline
        ]
        (purposeToString purpose)


encodePurpose : Purpose -> Value
encodePurpose =
    Util.enumEncoder allPurposes


encodeCondition : PurposeCondition -> Value
encodeCondition (PurposeList lps) =
    Encode.list <| List.map encodePurpose lps


purposeDecoder : Decoder Purpose
purposeDecoder =
    Util.enumDecoder "Purpose" allPurposes


conditionDecoder : Decoder PurposeCondition
conditionDecoder =
    Decode.map PurposeList <| Decode.list purposeDecoder
