module Data.Filter.Conditions.Region exposing
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
import Data.Validate as Validate
import Html exposing (Html)
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
    | SLOVENSKO


allRegions : List Region
allRegions =
    [ PRAHA
    , JIHOMORAVSKY
    , JIHOCESKY
    , PARDUBICKY
    , KRALOVEHRADECKY
    , VYSOCINA
    , KARLOVARSKY
    , LIBERECKY
    , OLOMOUCKY
    , PLZENKSY
    , STREDOCESKY
    , MORAVSKOSLEZSKY
    , USTECKY
    , ZLINSKY
    , SLOVENSKO
    ]


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

        SLOVENSKO ->
            "Slovensko"


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
    Validate.isNotEmpty "Kraj klienta" rlist


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
        |> List.indexedMap (\index region -> regionCheckbox index region (List.member region rlist))
        |> Html.div []


regionCheckbox : Int -> Region -> Bool -> Html RegionMsg
regionCheckbox index region isEnabled =
    Checkbox.checkbox
        [ Checkbox.id ("region_" ++ String.fromInt index)
        , Checkbox.checked isEnabled
        , Checkbox.inline
        , Checkbox.onCheck
            (\checked ->
                if checked then
                    AddRegion region

                else
                    RemoveRegion region
            )
        ]
        (regionToString region)



-- JSON


encodeRegion : Region -> Value
encodeRegion =
    Util.enumEncoder allRegions


encodeCondition : RegionCondition -> Value
encodeCondition (RegionList rs) =
    Encode.list encodeRegion rs


regionDecoder : Decoder Region
regionDecoder =
    Util.enumDecoder "Region" allRegions


conditionDecoder : Decoder RegionCondition
conditionDecoder =
    Decode.map RegionList <|
        Decode.list regionDecoder
