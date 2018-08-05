module Data.Filter.Conditions.Income
    exposing
        ( Income(..)
        , IncomeCondition(..)
        , IncomeMsg
        , allIncomes
        , conditionDecoder
        , defaultCondition
        , encodeCondition
        , form
        , renderCondition
        , update
        , validationErrors
        )

import Bootstrap.Form.Checkbox as Checkbox
import Html exposing (Html, div)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type Income
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
    = IncomeList (List Income)


defaultCondition : IncomeCondition
defaultCondition =
    IncomeList []


incomeToString : Income -> String
incomeToString income =
    case income of
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


allIncomes : List Income
allIncomes =
    [ EMPLOYMENT, ENTREPRENEUR, LIBERAL_PROFESSION, MATERNITY_LEAVE, PENSION, SELF_EMPLOYMENT, STUDENT, UNEMPLOYED, OTHER ]


renderCondition : IncomeCondition -> String
renderCondition (IncomeList list) =
    "klient je " ++ Util.orList incomeToString list


validationErrors : IncomeCondition -> List String
validationErrors (IncomeList rlist) =
    Util.validate (List.isEmpty rlist) "Zdroj příjmů klienta: zvolte aspoň jeden"


type IncomeMsg
    = AddIncome Income
    | RemoveIncome Income


update : IncomeMsg -> IncomeCondition -> IncomeCondition
update msg (IncomeList ilist) =
    case msg of
        AddIncome i ->
            IncomeList (i :: ilist)

        RemoveIncome i ->
            IncomeList (List.filter (\ii -> ii /= i) ilist)


form : IncomeCondition -> Html IncomeMsg
form (IncomeList plist) =
    allIncomes
        |> List.map (\p -> incomeCheckbox p (List.member p plist))
        |> div []


incomeCheckbox : Income -> Bool -> Html IncomeMsg
incomeCheckbox income isEnabled =
    Checkbox.checkbox
        [ Checkbox.id ("income_" ++ toString income)
        , Checkbox.onCheck
            (\checked ->
                if checked then
                    AddIncome income
                else
                    RemoveIncome income
            )
        , Checkbox.checked isEnabled
        , Checkbox.inline
        ]
        (incomeToString income)



-- JSON


encodeIncome : Income -> Value
encodeIncome =
    Util.enumEncoder allIncomes


encodeCondition : IncomeCondition -> Value
encodeCondition (IncomeList mis) =
    Encode.list <| List.map encodeIncome mis


incomeDecoder : Decoder Income
incomeDecoder =
    Util.enumDecoder "Income" allIncomes


conditionDecoder : Decoder IncomeCondition
conditionDecoder =
    Decode.map IncomeList <|
        Decode.list incomeDecoder
