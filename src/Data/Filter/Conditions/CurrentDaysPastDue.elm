module Data.Filter.Conditions.CurrentDaysPastDue exposing
    ( CurrentDaysPastDue(..)
    , CurrentDaysPastDueCondition(..)
    , CurrentDaysPastDueMsg
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
import Data.Filter.Constants as Constants
import Data.Validate as Validate
import DomId exposing (DomId)
import Html exposing (Html)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (parseInt, zeroToEmpty)
import View.NumericInput


type CurrentDaysPastDue
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type CurrentDaysPastDueCondition
    = CurrentDaysPastDueCondition CurrentDaysPastDue


defaultCondition : CurrentDaysPastDueCondition
defaultCondition =
    CurrentDaysPastDueCondition (LessThan 0)


currentDaysPastDueToString : CurrentDaysPastDue -> String
currentDaysPastDueToString currentDaysPastDue =
    case currentDaysPastDue of
        LessThan maxBound ->
            "nedosahuje " ++ String.fromInt maxBound

        Between minBound maxBound ->
            "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound

        MoreThan minBound ->
            "přesahuje " ++ String.fromInt minBound


renderCondition : CurrentDaysPastDueCondition -> String
renderCondition (CurrentDaysPastDueCondition condition) =
    "aktuální doba po splatnosti " ++ currentDaysPastDueToString condition ++ " dnů"


validationErrors : CurrentDaysPastDueCondition -> List String
validationErrors (CurrentDaysPastDueCondition t) =
    case t of
        LessThan x ->
            validatePositive x

        Between x y ->
            validatePositive x ++ validatePositive y ++ minNotGtMax x y

        MoreThan x ->
            validatePositive x


validatePositive : Int -> List String
validatePositive =
    Validate.intInRange "Aktuální doba po splatnosti ve dnech" 0 Constants.maxDaysDue


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Aktuální doba po splatnosti ve dnech"


type CurrentDaysPastDueMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | CurrentDaysPastDueNoOp


whichEnabled : CurrentDaysPastDue -> ( Bool, Bool, Bool )
whichEnabled currentDaysPastDue =
    case currentDaysPastDue of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


msgToModel : CurrentDaysPastDueMsg -> Maybe CurrentDaysPastDue
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        CurrentDaysPastDueNoOp ->
            Nothing


update : CurrentDaysPastDueMsg -> CurrentDaysPastDueCondition -> CurrentDaysPastDueCondition
update msg (CurrentDaysPastDueCondition cond) =
    msgToModel msg
        |> Maybe.withDefault cond
        |> CurrentDaysPastDueCondition


type alias CurrentDaysPastDueRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : CurrentDaysPastDueCondition -> Html CurrentDaysPastDueMsg
form (CurrentDaysPastDueCondition condition) =
    let
        values =
            case condition of
                LessThan x ->
                    CurrentDaysPastDueRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    CurrentDaysPastDueRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    CurrentDaysPastDueRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled condition
    in
    Html.div []
        [ Form.formInline [ onSubmit CurrentDaysPastDueNoOp ]
            [ currentDaysPastDueRadio ltEnabled (SetLessThan "0") "nedosahuje" "cdpd1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit CurrentDaysPastDueNoOp ]
            [ currentDaysPastDueRadio btwEnabled (SetBetween "0" "0") "je" "cdpd2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , Html.text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit CurrentDaysPastDueNoOp ]
            [ currentDaysPastDueRadio mtEnabled (SetMoreThan "0") "přesahuje" "cdpd3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    Html.text "dnů"


numericInput : (String -> CurrentDaysPastDueMsg) -> Bool -> String -> Html CurrentDaysPastDueMsg
numericInput =
    View.NumericInput.numericInput 0 Constants.maxDaysDue


currentDaysPastDueRadio : Bool -> CurrentDaysPastDueMsg -> String -> DomId -> Html CurrentDaysPastDueMsg
currentDaysPastDueRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "currentDaysPastDue"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeCurrentDaysPastDue : CurrentDaysPastDue -> Value
encodeCurrentDaysPastDue amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : CurrentDaysPastDueCondition -> Value
encodeCondition (CurrentDaysPastDueCondition c) =
    encodeCurrentDaysPastDue c


currentDaysPastDueDecoder : Decoder CurrentDaysPastDue
currentDaysPastDueDecoder =
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
                        Decode.fail <| "Unable to decode CurrentDaysPastDue from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder CurrentDaysPastDueCondition
conditionDecoder =
    Decode.map CurrentDaysPastDueCondition currentDaysPastDueDecoder
