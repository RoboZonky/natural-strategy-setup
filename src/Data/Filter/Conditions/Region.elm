module Data.Filter.Conditions.Region
    exposing
        ( Region(..)
        , RegionCondition(..)
        , RegionMsg
        , defaultRegionCondition
        , regionForm
        , renderRegionCondition
        , update
        , validationErrors
        )

import Bootstrap.Form.Checkbox as Checkbox
import Html exposing (Html, div)
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


defaultRegionCondition : RegionCondition
defaultRegionCondition =
    RegionList []


renderRegionCondition : RegionCondition -> String
renderRegionCondition (RegionList list) =
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


regionForm : RegionCondition -> Html RegionMsg
regionForm (RegionList rlist) =
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
