module Data.Filter.Conditions.LoanTerm
    exposing
        ( LoanTerm(..)
        , LoanTermCondition(..)
        , LoanTermMsg
        , defaultTermCondition
        , loanTermForm
        , renderLoanTermCondition
        , update
        , validationErrors
        )

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Html exposing (Attribute, Html, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onSubmit)
import Util exposing (emptyToZero, zeroToEmpty)


type LoanTerm
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type LoanTermCondition
    = LoanTermCondition LoanTerm


defaultTermCondition : LoanTermCondition
defaultTermCondition =
    LoanTermCondition (MoreThan 0)


loanTermToString : LoanTerm -> String
loanTermToString loanTerm =
    case loanTerm of
        LessThan maxBound ->
            "nedosahuje " ++ toString maxBound

        Between minBound maxBound ->
            "je " ++ toString minBound ++ " až " ++ toString maxBound

        MoreThan minBound ->
            "přesahuje " ++ toString minBound


renderLoanTermCondition : LoanTermCondition -> String
renderLoanTermCondition (LoanTermCondition term) =
    "délka " ++ loanTermToString term ++ " měsíců"


validationErrors : LoanTermCondition -> List String
validationErrors (LoanTermCondition t) =
    case t of
        LessThan x ->
            validateInt x

        Between x y ->
            validateInt x ++ validateInt y ++ validateMinNotGtMax x y

        MoreThan x ->
            validateInt x


validateInt : Int -> List String
validateInt x =
    Util.validate (x < 0 || 84 < x) "Délka úvěru: musí být v rozmezí 0 až 84"


validateMinNotGtMax : Int -> Int -> List String
validateMinNotGtMax minBound maxBound =
    Util.validate (minBound > maxBound) "Délka úvěru: minimum nesmí být větší než maximum"


type LoanTermMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | LoanTermNoOp


whichEnabled : LoanTerm -> ( Bool, Bool, Bool )
whichEnabled loanTerm =
    case loanTerm of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


update : LoanTermMsg -> LoanTermCondition -> LoanTermCondition
update msg (LoanTermCondition term) =
    case msg of
        SetLessThan hi ->
            emptyToZero hi |> String.toInt |> Result.map LessThan |> Result.withDefault term |> LoanTermCondition

        SetBetween loStr hiStr ->
            emptyToZero loStr
                |> String.toInt
                |> Result.andThen (\lo -> emptyToZero hiStr |> String.toInt |> Result.map (\hi -> Between lo hi))
                |> Result.withDefault term
                |> LoanTermCondition

        SetMoreThan lo ->
            emptyToZero lo |> String.toInt |> Result.map MoreThan |> Result.withDefault term |> LoanTermCondition

        LoanTermNoOp ->
            LoanTermCondition term


loanTermForm : LoanTermCondition -> Html LoanTermMsg
loanTermForm (LoanTermCondition loanTerm) =
    let
        ( ltVal, btwMinVal, btwMaxVal, mtVal ) =
            case loanTerm of
                LessThan x ->
                    ( zeroToEmpty x, "", "", "" )

                Between mi ma ->
                    ( "", zeroToEmpty mi, zeroToEmpty ma, "" )

                MoreThan x ->
                    ( "", "", "", zeroToEmpty x )

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled loanTerm
    in
    Form.form [ onSubmit LoanTermNoOp ]
        [ Form.formInline [ onSubmit LoanTermNoOp ]
            [ loanTermRadio ltEnabled (SetLessThan "0") "nedosahuje"
            , numericInput SetLessThan ltEnabled ltVal
            , text "měsíců"
            ]
        , Form.formInline [ onSubmit LoanTermNoOp ]
            [ loanTermRadio btwEnabled (SetBetween "0" "0") "je"
            , numericInput (\x -> SetBetween x btwMaxVal) btwEnabled btwMinVal
            , text "až"
            , numericInput (\y -> SetBetween btwMinVal y) btwEnabled btwMaxVal
            , text "měsíců"
            ]
        , Form.formInline [ onSubmit LoanTermNoOp ]
            [ loanTermRadio mtEnabled (SetMoreThan "0") "přesahuje"
            , numericInput SetMoreThan mtEnabled mtVal
            , text "měsíců"
            ]
        ]


numericInput : (String -> LoanTermMsg) -> Bool -> String -> Html LoanTermMsg
numericInput msg enabled value =
    Input.number
        [ Input.small
        , Input.onInput msg
        , Input.disabled <| not enabled
        , Input.value value
        , Input.attrs [ Attr.min "0", Attr.max "100", class "mx-1" ]
        ]


loanTermRadio : Bool -> LoanTermMsg -> String -> Html LoanTermMsg
loanTermRadio checked msg label =
    Radio.radio
        [ Radio.name "loanTerm"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label
