module Data.Interest exposing (..)


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
            "nedosahuje " ++ toString maxBound

        Between minBound maxBound ->
            "je " ++ toString minBound ++ " až " ++ toString maxBound

        MoreThan minBound ->
            "přesahuje " ++ toString minBound


renderInterestCondition : InterestCondition -> String
renderInterestCondition (InterestCondition interest) =
    "úrok " ++ interestToString interest ++ " % p.a."
