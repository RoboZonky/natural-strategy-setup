module Data.Filter.Condition.Amount
    exposing
        ( Amount(..)
        , AmountCondition(..)
        , AmountMsg(..)
        , amountForm
        , defaultAmountCondition
        , map
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


map : (Amount -> Amount) -> AmountCondition -> AmountCondition
map f (AmountCondition c) =
    AmountCondition (f c)


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


isLess : Amount -> Bool
isLess amt =
    case amt of
        LessThan _ ->
            True

        _ ->
            False


isBetween : Amount -> Bool
isBetween amt =
    case amt of
        Between _ _ ->
            True

        _ ->
            False


isMore : Amount -> Bool
isMore amt =
    case amt of
        MoreThan _ ->
            True

        _ ->
            False


update : AmountMsg -> Amount -> Amount
update msg amt =
    case msg of
        SetLessThan hi ->
            emptyToZero hi |> String.toInt |> Result.map LessThan |> Result.withDefault amt

        SetBetween loStr hiStr ->
            emptyToZero loStr
                |> String.toInt
                |> Result.andThen (\lo -> emptyToZero hiStr |> String.toInt |> Result.map (\hi -> Between lo hi))
                |> Result.withDefault amt

        SetMoreThan lo ->
            emptyToZero lo |> String.toInt |> Result.map MoreThan |> Result.withDefault amt

        AmountNoOp ->
            amt


amountForm : Amount -> Html AmountMsg
amountForm amt =
    let
        ltVal =
            case amt of
                LessThan x ->
                    zeroToEmpty x

                _ ->
                    ""

        btwMinVal =
            case amt of
                Between mi _ ->
                    zeroToEmpty mi

                _ ->
                    ""

        btwMaxVal =
            case amt of
                Between _ ma ->
                    zeroToEmpty ma

                _ ->
                    ""

        mtVal =
            case amt of
                MoreThan x ->
                    zeroToEmpty x

                _ ->
                    ""
    in
    Form.form [ onSubmit AmountNoOp ]
        [ Form.formInline [ onSubmit AmountNoOp ]
            [ amountRadio (isLess amt) (SetLessThan "0") "nedosahuje"
            , numericInput SetLessThan (isLess amt) ltVal
            , text "Kč"
            ]
        , Form.formInline [ onSubmit AmountNoOp ]
            [ amountRadio (isBetween amt) (SetBetween "0" "0") "je"
            , numericInput (\x -> SetBetween x btwMaxVal) (isBetween amt) btwMinVal
            , text "až"
            , numericInput (\y -> SetBetween btwMinVal y) (isBetween amt) btwMaxVal
            , text "Kč"
            ]
        , Form.formInline [ onSubmit AmountNoOp ]
            [ amountRadio (isMore amt) (SetMoreThan "0") "přesahuje"
            , numericInput SetMoreThan (isMore amt) mtVal
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
