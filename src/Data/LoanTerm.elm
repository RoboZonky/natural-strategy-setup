module Data.LoanTerm
    exposing
        ( LoanTerm(..)
        , TermCondition(..)
        , renderTermCondition
        )


type LoanTerm
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type TermCondition
    = TermCondition LoanTerm


loanTermToString : LoanTerm -> String
loanTermToString loanTerm =
    case loanTerm of
        LessThan maxBound ->
            "nedosahuje " ++ toString maxBound

        Between minBound maxBound ->
            "je " ++ toString minBound ++ " až " ++ toString maxBound

        MoreThan minBound ->
            "přesahuje " ++ toString minBound


renderTermCondition : TermCondition -> String
renderTermCondition (TermCondition term) =
    "délka " ++ loanTermToString term ++ " měsíců"
