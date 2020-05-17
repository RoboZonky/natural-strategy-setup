module Data.Filter.Constants exposing (maxDaysDue)

{-| 8 years. Maximum 7 years, plus an estimated 1 year of possible delinquency on top.
-}


maxDaysDue : Int
maxDaysDue =
    8 * 365
