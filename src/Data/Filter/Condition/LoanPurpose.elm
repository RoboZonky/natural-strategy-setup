module Data.Filter.Condition.LoanPurpose
    exposing
        ( LoanPurpose(..)
        , LoanPurposeCondition(..)
        , renderLoanPurposeCondition
        )

import Util


type LoanPurpose
    = AUTO_MOTO
    | CESTOVANI
    | DOMACNOST
    | ELEKTRONIKA
    | REFINANCOVANI_PUJCEK
    | VLASTNI_PROJEKT
    | VZDELANI
    | ZDRAVI
    | JINE


type LoanPurposeCondition
    = LoanPurposeList (List LoanPurpose)


loanPurposeToString : LoanPurpose -> String
loanPurposeToString loanPurpose =
    case loanPurpose of
        AUTO_MOTO ->
            "auto-moto"

        CESTOVANI ->
            "cestování"

        DOMACNOST ->
            "domácnost"

        ELEKTRONIKA ->
            "elektronika"

        REFINANCOVANI_PUJCEK ->
            "refinancování půjček"

        VLASTNI_PROJEKT ->
            "vlastní projekt"

        VZDELANI ->
            "vzdělání"

        ZDRAVI ->
            "zdraví"

        JINE ->
            "jiné"


renderLoanPurposeCondition : LoanPurposeCondition -> String
renderLoanPurposeCondition (LoanPurposeList list) =
    "účel je" ++ renderLoandPurposeList list


renderLoandPurposeList : List LoanPurpose -> String
renderLoandPurposeList =
    Util.orList loanPurposeToString
