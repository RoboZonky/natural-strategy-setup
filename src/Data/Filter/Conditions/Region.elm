module Data.Filter.Conditions.Region
    exposing
        ( Region(..)
        , RegionCondition(..)
        , RegionMsg
        , allRegions
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


type Region
    = PRAHA
    | JIHOMORAVSKY
    | JIHOCESKY
    | PARDUBICKY
    | KRALOVEHRADECKY
    | VYSOCINA
    | KARLOVARSKY
    | LIBERECKY
    | OLOMOUCKY
    | PLZENKSY
    | STREDOCESKY
    | MORAVSKOSLEZSKY
    | USTECKY
    | ZLINSKY


allRegions : List Region
allRegions =
    [ PRAHA, JIHOMORAVSKY, JIHOCESKY, PARDUBICKY, KRALOVEHRADECKY, VYSOCINA, KARLOVARSKY, LIBERECKY, OLOMOUCKY, PLZENKSY, STREDOCESKY, MORAVSKOSLEZSKY, USTECKY, ZLINSKY ]


regionToString : Region -> String
regionToString r =
    case r of
        PRAHA ->
            "Praha"

        JIHOMORAVSKY ->
            "Jihomoravský"

        JIHOCESKY ->
            "Jihočeský"

        PARDUBICKY ->
            "Pardubický"

        KRALOVEHRADECKY ->
            "Královéhradecký"

        VYSOCINA ->
            "Vysočina"

        KARLOVARSKY ->
            "Karlovarský"

        LIBERECKY ->
            "Liberecký"

        OLOMOUCKY ->
            "Olomoucký"

        PLZENKSY ->
            "Plzeňský"

        STREDOCESKY ->
            "Středočeský"

        MORAVSKOSLEZSKY ->
            "Moravskoslezský"

        USTECKY ->
            "Ústecký"

        ZLINSKY ->
            "Zlínský"


type RegionCondition
    = RegionList (List Region)


defaultCondition : RegionCondition
defaultCondition =
    RegionList []


renderCondition : RegionCondition -> String
renderCondition (RegionList list) =
    "kraj klienta je " ++ renderRegionList list


renderRegionList : List Region -> String
renderRegionList =
    Util.orList regionToString


validationErrors : RegionCondition -> List String
validationErrors (RegionList rlist) =
    Util.validate (List.isEmpty rlist) "Kraj klienta: zvolte aspoň jeden"


type RegionMsg
    = AddRegion Region
    | RemoveRegion Region


update : RegionMsg -> RegionCondition -> RegionCondition
update msg (RegionList rlist) =
    case msg of
        AddRegion r ->
            RegionList (r :: rlist)

        RemoveRegion r ->
            RegionList (List.filter (\ru -> ru /= r) rlist)


form : RegionCondition -> Html RegionMsg
form (RegionList rlist) =
    allRegions
        |> List.map (\p -> regionCheckbox p (List.member p rlist))
        |> div []


regionCheckbox : Region -> Bool -> Html RegionMsg
regionCheckbox region isEnabled =
    Checkbox.checkbox
        [ Checkbox.onCheck
            (\checked ->
                if checked then
                    AddRegion region
                else
                    RemoveRegion region
            )
        , Checkbox.checked isEnabled
        , Checkbox.inline
        ]
        (regionToString region)



-- JSON


encodeRegion : Region -> Value
encodeRegion =
    Encode.string << toString


encodeCondition : RegionCondition -> Value
encodeCondition (RegionList rs) =
    Encode.list <| List.map encodeRegion rs


regionDecoder : Decoder Region
regionDecoder =
    Util.enumDecoder allRegions


conditionDecoder : Decoder RegionCondition
conditionDecoder =
    Decode.map RegionList <|
        Decode.list regionDecoder
