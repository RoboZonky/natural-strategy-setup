module Data.Filter.Conditions.MainIncome
    exposing
        ( MainIncome(..)
        , MainIncomeCondition(..)
        , MainIncomeMsg
        , allIncomes
        , conditionDecoder
        , defaultIncomeCondition
        , encodeCondition
        , mainIncomeForm
        , renderMainIncomeCondition
        , update
        , validationErrors
        )

import Bootstrap.Form.Checkbox as Checkbox
import Html exposing (Html, div)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
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


type MainIncomeCondition
    = MainIncomeList (List MainIncome)


defaultIncomeCondition : MainIncomeCondition
defaultIncomeCondition =
    MainIncomeList []


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


allIncomes : List MainIncome
allIncomes =
    [ EMPLOYMENT, ENTREPRENEUR, LIBERAL_PROFESSION, MATERNITY_LEAVE, PENSION, SELF_EMPLOYMENT, STUDENT, UNEMPLOYED, OTHER ]


renderMainIncomeCondition : MainIncomeCondition -> String
renderMainIncomeCondition (MainIncomeList list) =
    "klient je " ++ Util.orList mainIncomeToString list


validationErrors : MainIncomeCondition -> List String
validationErrors (MainIncomeList rlist) =
    Util.validate (List.isEmpty rlist) "Zdroj příjmů klienta: zvolte aspoň jeden"


type MainIncomeMsg
    = AddMainIncome MainIncome
    | RemoveMainIncome MainIncome


update : MainIncomeMsg -> MainIncomeCondition -> MainIncomeCondition
update msg (MainIncomeList ilist) =
    case msg of
        AddMainIncome i ->
            MainIncomeList (i :: ilist)

        RemoveMainIncome i ->
            MainIncomeList (List.filter (\ii -> ii /= i) ilist)


mainIncomeForm : MainIncomeCondition -> Html MainIncomeMsg
mainIncomeForm (MainIncomeList plist) =
    allIncomes
        |> List.map (\p -> mainIncomeCheckbox p (List.member p plist))
        |> div []


mainIncomeCheckbox : MainIncome -> Bool -> Html MainIncomeMsg
mainIncomeCheckbox income isEnabled =
    Checkbox.checkbox
        [ Checkbox.onCheck
            (\checked ->
                if checked then
                    AddMainIncome income
                else
                    RemoveMainIncome income
            )
        , Checkbox.checked isEnabled
        , Checkbox.inline
        ]
        (mainIncomeToString income)



-- JSON


encodeMainIncome : MainIncome -> Value
encodeMainIncome =
    Encode.string << toString


encodeCondition : MainIncomeCondition -> Value
encodeCondition (MainIncomeList mis) =
    Encode.list <| List.map encodeMainIncome mis


mainIncomeDecoder : Decoder MainIncome
mainIncomeDecoder =
    Util.enumDecoder allIncomes


conditionDecoder : Decoder MainIncomeCondition
conditionDecoder =
    Decode.map MainIncomeList <|
        Decode.list mainIncomeDecoder
