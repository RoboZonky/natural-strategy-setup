module Data.Filter.Conditions.Amount
    exposing
        ( Amount(Between, LessThan, MoreThan)
        , AmountCondition(AmountCondition)
        , AmountMsg
        , conditionDecoder
        , defaultCondition
        , encodeCondition
        , form
        , renderCondition
        , update
        , validationErrors
        )

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Html exposing (Html, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (emptyToZero, zeroToEmpty)


type Amount
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type AmountCondition
    = AmountCondition Amount


defaultCondition : AmountCondition
defaultCondition =
    AmountCondition (LessThan 0)


renderCondition : AmountCondition -> String
renderCondition (AmountCondition amount) =
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


validationErrors : AmountCondition -> List String
validationErrors (AmountCondition a) =
    case a of
        LessThan x ->
            validateInt x

        Between x y ->
            validateInt x ++ validateInt y ++ validateMinNotGtMax x y

        MoreThan x ->
            validateInt x


validateInt : Int -> List String
validateInt x =
    Util.validate (x < 0) "Výše úvěru: musí být kladné číslo"


validateMinNotGtMax : Int -> Int -> List String
validateMinNotGtMax minBound maxBound =
    Util.validate (minBound > maxBound) "Výše úvěru: minimum nesmí být větší než maximum"


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
update msg ac =
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


form : AmountCondition -> Html AmountMsg
form (AmountCondition amt) =
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



-- JSON


encodeAmount : Amount -> Value
encodeAmount amt =
    case amt of
        LessThan x ->
            Encode.list [ Encode.int 1, Encode.int x ]

        Between x y ->
            Encode.list [ Encode.int 2, Encode.int x, Encode.int y ]

        MoreThan y ->
            Encode.list [ Encode.int 3, Encode.int y ]


encodeCondition : AmountCondition -> Value
encodeCondition (AmountCondition c) =
    encodeAmount c


amountDecoder : Decoder Amount
amountDecoder =
    Decode.list Decode.int
        |> Decode.andThen
            (\ints ->
                case ints of
                    [ 1, x ] ->
                        Decode.succeed <| LessThan x

                    [ 2, x, y ] ->
                        Decode.succeed <| Between x y

                    [ 3, y ] ->
                        Decode.succeed <| MoreThan y

                    _ ->
                        Decode.fail <| "Unable to decode Amount from " ++ toString ints
            )


conditionDecoder : Decoder AmountCondition
conditionDecoder =
    Decode.map AmountCondition amountDecoder
