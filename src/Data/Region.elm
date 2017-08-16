module Data.Region
    exposing
        ( Region(..)
        , RegionFilter(..)
        , renderRegionFilter
        )

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


type RegionFilter
    = RegionList (List Region)


renderRegionFilter : RegionFilter -> String
renderRegionFilter (RegionList list) =
    "kraj klienta je " ++ renderRegionList list


renderRegionList : List Region -> String
renderRegionList =
    Util.orList regionToString
