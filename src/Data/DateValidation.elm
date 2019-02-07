module Data.DateValidation exposing (isValidDate)

-- Migration to elm 0.19 meant that elm-community/elm-time could no longer used
-- This validation function was stolen from https://github.com/elm-community/elm-time/blob/3.0.5/src/Time/Date.elm


isValidDate : Int -> Int -> Int -> Bool
isValidDate year month day =
    daysInMonth year month
        |> Maybe.map (\days -> day >= 1 && day <= days)
        |> Maybe.withDefault False


daysInMonth : Int -> Int -> Maybe Int
daysInMonth y m =
    if m == 1 then
        Just 31

    else if m == 2 && isLeapYear y then
        Just 29

    else if m == 2 then
        Just 28

    else if m == 3 then
        Just 31

    else if m == 4 then
        Just 30

    else if m == 5 then
        Just 31

    else if m == 6 then
        Just 30

    else if m == 7 then
        Just 31

    else if m == 8 then
        Just 31

    else if m == 9 then
        Just 30

    else if m == 10 then
        Just 31

    else if m == 11 then
        Just 30

    else if m == 12 then
        Just 31

    else
        Nothing


isLeapYear : Int -> Bool
isLeapYear y =
    modBy 400 y == 0 || modBy 100 y /= 0 && modBy 4 y == 0
