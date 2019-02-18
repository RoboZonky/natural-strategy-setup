module Data.Validate exposing
    ( intInRange
    , isNotEmpty
    , minNotGtMax
    , positive
    , validate
    )


validate : Bool -> String -> List String
validate errorCondition error =
    if errorCondition then
        [ error ]

    else
        []


positive : String -> number -> List String
positive what x =
    validate (x < 0)
        (what ++ ": musí být kladné číslo")


minNotGtMax : String -> number -> number -> List String
minNotGtMax what minBound maxBound =
    validate (minBound > maxBound)
        (what ++ ": minimum nesmí být větší než maximum")


isNotEmpty : String -> List a -> List String
isNotEmpty what list =
    validate (List.isEmpty list)
        (what ++ ": zvolte aspoň jeden")


intInRange : String -> Int -> Int -> Int -> List String
intInRange what minValid maxValid x =
    validate (x < minValid || maxValid < x)
        (what
            ++ ": musí být v rozmezí "
            ++ String.fromInt minValid
            ++ " až "
            ++ String.fromInt maxValid
        )
