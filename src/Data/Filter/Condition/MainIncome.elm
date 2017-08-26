module Data.Filter.Condition.MainIncome
    exposing
        ( IncomeCondition(..)
        , MainIncome(..)
        , renderIncomeCondition
        )

import Util


type MainIncome
    = EMPLOYMENT
    | ENTREPRENEUR
    | LIBERAL_PROFESSION
    | MATERNITY_LEAVE
    | PENSION
    | SELF_EMPLOYMENT
    | STUDENT
    | UNEMPLOYED
    | OTHER


type IncomeCondition
    = IncomeList (List MainIncome)


mainIncomeToString : MainIncome -> String
mainIncomeToString mainIncome =
    case mainIncome of
        EMPLOYMENT ->
            "zaměstnanec"

        ENTREPRENEUR ->
            "podnikatel"

        LIBERAL_PROFESSION ->
            "svobodné povolání"

        MATERNITY_LEAVE ->
            "na rodičovské dovolené"

        PENSION ->
            "důchodce"

        SELF_EMPLOYMENT ->
            "OSVČ"

        STUDENT ->
            "student"

        UNEMPLOYED ->
            "bez zaměstnání"

        OTHER ->
            "jiné"


renderIncomeCondition : IncomeCondition -> String
renderIncomeCondition (IncomeList list) =
    "klient je " ++ renderMainIncomeList list


renderMainIncomeList : List MainIncome -> String
renderMainIncomeList =
    Util.orList mainIncomeToString
