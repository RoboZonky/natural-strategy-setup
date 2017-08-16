module Data.Strategy.Region exposing (..)


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
renderRegionList rs =
    case rs of
        [] ->
            "VYBERTE ASPOŇ JEDNU HODNOTU"

        r :: [] ->
            regionToString r

        r1 :: r2 :: [] ->
            regionToString r1 ++ " nebo " ++ regionToString r2

        r1 :: rest ->
            regionToString r1 ++ ", " ++ renderRegionList rest
