module Data.Filter.Condition.Amount
    exposing
        ( Amount(..)
        , AmountCondition(..)
        , AmountMsg
        , amountForm
        , defaultAmountCondition
        , renderAmountCondition
        , update
        )

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Html exposing (Attribute, Html, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onSubmit)
import Util exposing (emptyToZero, zeroToEmpty)


type Amount
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type AmountCondition
    = AmountCondition Amount


defaultAmountCondition : AmountCondition
defaultAmountCondition =
    AmountCondition (MoreThan 0)


renderAmountCondition : AmountCondition -> String
renderAmountCondition (AmountCondition amount) =
    "výše " ++ amountToString amount ++ " Kč"


amountToString : Amount -> String
amountToString amount =
    case amount of
        Between from to ->
            "je " ++ toString from ++ " až " ++ toString to

        MoreThan lowerBound ->
            "přesahuje " ++ toString lowerBound

        LessThan upperBound ->
            "nedosahuje " ++ toString upperBound


type AmountMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | AmountNoOp


whichEnabled : Amount -> ( Bool, Bool, Bool )
whichEnabled amt =
    case amt of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


update : AmountMsg -> AmountCondition -> AmountCondition
update msg ((AmountCondition amt) as ac) =
    case msg of
        SetLessThan hi ->
            emptyToZero hi |> String.toInt |> Result.map (AmountCondition << LessThan) |> Result.withDefault ac

        SetBetween loStr hiStr ->
            emptyToZero loStr
                |> String.toInt
                |> Result.andThen (\lo -> emptyToZero hiStr |> String.toInt |> Result.map (\hi -> AmountCondition <| Between lo hi))
                |> Result.withDefault ac

        SetMoreThan lo ->
            emptyToZero lo |> String.toInt |> Result.map (AmountCondition << MoreThan) |> Result.withDefault ac

        AmountNoOp ->
            ac


amountForm : AmountCondition -> Html AmountMsg
amountForm (AmountCondition amt) =
    let
        ( ltVal, btwMinVal, btwMaxVal, mtVal ) =
            case amt of
                LessThan x ->
                    ( zeroToEmpty x, "", "", "" )

                Between mi ma ->
                    ( "", zeroToEmpty mi, zeroToEmpty ma, "" )

                MoreThan x ->
                    ( "", "", "", zeroToEmpty x )

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled amt
    in
    Form.form [ onSubmit AmountNoOp ]
        [ Form.formInline [ onSubmit AmountNoOp ]
            [ amountRadio ltEnabled (SetLessThan "0") "nedosahuje"
            , numericInput SetLessThan ltEnabled ltVal
            , text "Kč"
            ]
        , Form.formInline [ onSubmit AmountNoOp ]
            [ amountRadio btwEnabled (SetBetween "0" "0") "je"
            , numericInput (\x -> SetBetween x btwMaxVal) btwEnabled btwMinVal
            , text "až"
            , numericInput (\y -> SetBetween btwMinVal y) btwEnabled btwMaxVal
            , text "Kč"
            ]
        , Form.formInline [ onSubmit AmountNoOp ]
            [ amountRadio mtEnabled (SetMoreThan "0") "přesahuje"
            , numericInput SetMoreThan mtEnabled mtVal
            , text "Kč"
            ]
        ]


numericInput : (String -> AmountMsg) -> Bool -> String -> Html AmountMsg
numericInput msg enabled value =
    Input.number
        [ Input.small
        , Input.onInput msg
        , Input.disabled <| not enabled
        , Input.value value
        , Input.attrs [ Attr.min "0", Attr.max "10000000", class "mx-1" ]
        ]


amountRadio : Bool -> AmountMsg -> String -> Html AmountMsg
amountRadio checked msg label =
    Radio.radio
        [ Radio.name "amount"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label
