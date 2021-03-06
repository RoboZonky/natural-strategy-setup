module Data.Filter.Conditions.Amount exposing
    ( Amount(..)
    , AmountCondition(..)
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
import Bootstrap.Form.Radio as Radio
import Data.Filter.Constants exposing (maxLoanAmount)
import Data.Validate as Validate
import DomId exposing (DomId)
import Html exposing (Html)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (parseInt, zeroToEmpty)
import View.NumericInput


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
            "je " ++ String.fromInt from ++ " až " ++ String.fromInt to

        MoreThan lowerBound ->
            "přesahuje " ++ String.fromInt lowerBound

        LessThan upperBound ->
            "nedosahuje " ++ String.fromInt upperBound


validationErrors : AmountCondition -> List String
validationErrors (AmountCondition a) =
    case a of
        LessThan x ->
            validAmount x

        Between x y ->
            validAmount x ++ validAmount y ++ minNotGtMax x y

        MoreThan x ->
            validAmount x


validAmount : Int -> List String
validAmount =
    Validate.intInRange "Výše půjčky" 0 maxLoanAmount


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Výše půjčky"


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


msgToModel : AmountMsg -> Maybe Amount
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        AmountNoOp ->
            Nothing


update : AmountMsg -> AmountCondition -> AmountCondition
update msg (AmountCondition amt) =
    msgToModel msg
        |> Maybe.withDefault amt
        |> AmountCondition


type alias AmountRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : AmountCondition -> Html AmountMsg
form (AmountCondition amt) =
    let
        values =
            case amt of
                LessThan x ->
                    AmountRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    AmountRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    AmountRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled amt
    in
    Html.div []
        [ Form.formInline [ onSubmit AmountNoOp ]
            [ amountRadio ltEnabled (SetLessThan "0") "nedosahuje" "amount1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit AmountNoOp ]
            [ amountRadio btwEnabled (SetBetween "0" "0") "je" "amount2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , Html.text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit AmountNoOp ]
            [ amountRadio mtEnabled (SetMoreThan "0") "přesahuje" "amount3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    Html.text "Kč"


numericInput : (String -> AmountMsg) -> Bool -> String -> Html AmountMsg
numericInput =
    View.NumericInput.numericInput 0 maxLoanAmount


amountRadio : Bool -> AmountMsg -> String -> DomId -> Html AmountMsg
amountRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "amount"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeAmount : Amount -> Value
encodeAmount amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


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
                        Decode.fail <| "Unable to decode Amount from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder AmountCondition
conditionDecoder =
    Decode.map AmountCondition amountDecoder
