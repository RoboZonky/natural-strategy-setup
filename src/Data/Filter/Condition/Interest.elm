module Data.Filter.Condition.Interest exposing (..)


type Interest
    = LessThan Float
    | Between Float Float
    | MoreThan Float


type InterestCondition
    = InterestCondition Interest


interestToString : Interest -> String
interestToString interest =
    case interest of
        LessThan maxBound ->
            "nedosahuje " ++ floatToString maxBound

        Between minBound maxBound ->
            "je " ++ floatToString minBound ++ " až " ++ floatToString maxBound

        MoreThan minBound ->
            "přesahuje " ++ floatToString minBound


renderInterestCondition : InterestCondition -> String
renderInterestCondition (InterestCondition interest) =
    "úrok " ++ interestToString interest ++ " % p.a"


floatToString : Float -> String
floatToString =
    -- toString for float has '.'. Replace it with ','
    String.map
        (\c ->
            if c == '.' then
                ','
            else
                c
        )
        << toString
