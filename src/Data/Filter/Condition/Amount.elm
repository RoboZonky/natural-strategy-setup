module Data.Filter.Condition.Amount
    exposing
        ( Amount(..)
        , AmountCondition(..)
        , defaultAmountCondition
        , renderAmountCondition
        )

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Html exposing (Html, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onSubmit)


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


type alias Model =
    Amount


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
    amt


amountForm : Model -> Html AmountMsg
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
    in
    Form.formInline [ onSubmit AmountNoOp ]
        [ Radio.radio
            [ Radio.checked (isLess amt)
            , Radio.name "amount"
            , Radio.onClick (SetLessThan "0")
            ]
            "nedosahuje"
        , Input.number
            [ Input.small
            , Input.onInput SetLessThan
            , Input.disabled (isLess amt)
            , Input.value ltVal
            , Input.attrs [ Attr.min "0", Attr.max "100000000", class "mx-1" ]
            ]
        , Radio.radio
            [ Radio.checked (isBetween amt)
            , Radio.name "amount"
            , Radio.onClick (SetLessThan "0")
            ]
            "je"
        , Input.number
            [ Input.small
            , Input.onInput (\x -> SetBetween x btwMaxVal)
            , Input.disabled (isBetween amt)
            , Input.value btwMinVal
            , Input.attrs [ Attr.min "0", Attr.max "100000000", class "mx-1" ]
            ]
        , text "až"
        , Input.number
            [ Input.small
            , Input.onInput (\y -> SetBetween btwMinVal y)
            , Input.disabled (isBetween amt)
            , Input.value btwMaxVal
            , Input.attrs [ Attr.min "0", Attr.max "100000000", class "mx-1" ]
            ]
        , text "Kč."
        ]


zeroToEmpty : Int -> String
zeroToEmpty x =
    if x == 0 then
        ""
    else
        toString x
