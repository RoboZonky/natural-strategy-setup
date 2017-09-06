module Data.Filter.Condition.Interest
    exposing
        ( Interest(..)
        , InterestCondition(..)
        , InterestMsg
        , interestForm
        , interestToString
        , map
        , renderInterestCondition
        , update
        )

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Html exposing (Html, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onSubmit)
import Util exposing (emptyToZero, zeroToEmpty)


type Interest
    = LessThan Float
    | Between Float Float
    | MoreThan Float


type InterestCondition
    = InterestCondition Interest


map : (Interest -> Interest) -> InterestCondition -> InterestCondition
map f (InterestCondition c) =
    InterestCondition (f c)


defaultInterestCondition : InterestCondition
defaultInterestCondition =
    InterestCondition (MoreThan 0)


interestToString : Interest -> String
interestToString interest =
    case interest of
        LessThan maxBound ->
            "nedosahuje " ++ floatToString maxBound

        Between minBound maxBound ->
            "je " ++ floatToString minBound ++ " až " ++ floatToString maxBound

        MoreThan minBound ->
            "přesahuje " ++ floatToString minBound


renderInterestCondition : InterestCondition -> String
renderInterestCondition (InterestCondition interest) =
    "úrok " ++ interestToString interest ++ " % p.a"


floatToString : Float -> String
floatToString =
    -- toString for float has '.'. Replace it with ','
    String.map
        (\c ->
            if c == '.' then
                ','
            else
                c
        )
        << toString


type InterestMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | InterestNoOp


isLess : Interest -> Bool
isLess amt =
    case amt of
        LessThan _ ->
            True

        _ ->
            False


isBetween : Interest -> Bool
isBetween amt =
    case amt of
        Between _ _ ->
            True

        _ ->
            False


isMore : Interest -> Bool
isMore amt =
    case amt of
        MoreThan _ ->
            True

        _ ->
            False


update : InterestMsg -> Interest -> Interest
update msg amt =
    case msg of
        SetLessThan hi ->
            emptyToZero hi |> String.toFloat |> Result.map LessThan |> Result.withDefault amt

        SetBetween loStr hiStr ->
            emptyToZero loStr
                |> String.toFloat
                |> Result.andThen (\lo -> emptyToZero hiStr |> String.toFloat |> Result.map (\hi -> Between lo hi))
                |> Result.withDefault amt

        SetMoreThan lo ->
            emptyToZero lo |> String.toFloat |> Result.map MoreThan |> Result.withDefault amt

        InterestNoOp ->
            amt


interestForm : Interest -> Html InterestMsg
interestForm amt =
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
    Form.form [ onSubmit InterestNoOp ]
        [ Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio (isLess amt) (SetLessThan "0") "nedosahuje"
            , numericInput SetLessThan (isLess amt) ltVal
            , text "Kč"
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio (isBetween amt) (SetBetween "0" "0") "je"
            , numericInput (\x -> SetBetween x btwMaxVal) (isBetween amt) btwMinVal
            , text "až"
            , numericInput (\y -> SetBetween btwMinVal y) (isBetween amt) btwMaxVal
            , text "Kč"
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio (isMore amt) (SetMoreThan "0") "přesahuje"
            , numericInput SetMoreThan (isMore amt) mtVal
            , text "Kč"
            ]
        ]


numericInput : (String -> InterestMsg) -> Bool -> String -> Html InterestMsg
numericInput msg enabled value =
    Input.number
        [ Input.small
        , Input.onInput msg
        , Input.disabled <| not enabled
        , Input.value value
        , Input.attrs [ Attr.min "0", Attr.max "10000000", class "mx-1" ]
        ]


interestRadio : Bool -> InterestMsg -> String -> Html InterestMsg
interestRadio checked msg label =
    Radio.radio
        [ Radio.name "interest"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label
