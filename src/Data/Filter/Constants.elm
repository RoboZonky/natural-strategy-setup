module Data.Filter.Constants exposing (maxDaysDue, maxTermMonths, minTermMonths)

{-| Maximum 10 years, plus an estimated 1 year of possible delinquency on top.
-}


maxDaysDue : Int
maxDaysDue =
    11 * 365


minTermMonths : Int
minTermMonths =
    0


maxTermMonths : Int
maxTermMonths =
    120
