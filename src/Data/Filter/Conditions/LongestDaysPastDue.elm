module Data.Filter.Conditions.LongestDaysPastDue exposing
    ( LongestDaysPastDue(..)
    , LongestDaysPastDueCondition(..)
    , LongestDaysPastDueMsg
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


type LongestDaysPastDue
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type LongestDaysPastDueCondition
    = LongestDaysPastDueCondition LongestDaysPastDue


defaultCondition : LongestDaysPastDueCondition
defaultCondition =
    LongestDaysPastDueCondition (LessThan 0)


longestDaysPastDueToString : LongestDaysPastDue -> String
longestDaysPastDueToString longestDaysPastDue =
    case longestDaysPastDue of
        LessThan maxBound ->
            "nedosahuje " ++ String.fromInt maxBound

        Between minBound maxBound ->
            "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound

        MoreThan minBound ->
            "přesahuje " ++ String.fromInt minBound


renderCondition : LongestDaysPastDueCondition -> String
renderCondition (LongestDaysPastDueCondition condition) =
    "nejdelší doba po splatnosti " ++ longestDaysPastDueToString condition ++ " dnů"


validationErrors : LongestDaysPastDueCondition -> List String
validationErrors (LongestDaysPastDueCondition t) =
    case t of
        LessThan x ->
            validateInRange x

        Between x y ->
            validateInRange x ++ validateInRange y ++ minNotGtMax x y

        MoreThan x ->
            validateInRange x


validateInRange : Int -> List String
validateInRange =
    Validate.intInRange "Nejdelší doba po splatnosti ve dnech" 0 Constants.maxDaysDue


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Nejdelší doba po splatnosti ve dnech"


type LongestDaysPastDueMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | LongestDaysPastDueNoOp


whichEnabled : LongestDaysPastDue -> ( Bool, Bool, Bool )
whichEnabled longestDaysPastDue =
    case longestDaysPastDue of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


msgToModel : LongestDaysPastDueMsg -> Maybe LongestDaysPastDue
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        LongestDaysPastDueNoOp ->
            Nothing


update : LongestDaysPastDueMsg -> LongestDaysPastDueCondition -> LongestDaysPastDueCondition
update msg (LongestDaysPastDueCondition cond) =
    msgToModel msg
        |> Maybe.withDefault cond
        |> LongestDaysPastDueCondition


type alias LongestDaysPastDueRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : LongestDaysPastDueCondition -> Html LongestDaysPastDueMsg
form (LongestDaysPastDueCondition condition) =
    let
        values =
            case condition of
                LessThan x ->
                    LongestDaysPastDueRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    LongestDaysPastDueRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    LongestDaysPastDueRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled condition
    in
    Html.div []
        [ Form.formInline [ onSubmit LongestDaysPastDueNoOp ]
            [ longestDaysPastDueRadio ltEnabled (SetLessThan "0") "nedosahuje" "ldpd1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit LongestDaysPastDueNoOp ]
            [ longestDaysPastDueRadio btwEnabled (SetBetween "0" "0") "je" "ldpd2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , Html.text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit LongestDaysPastDueNoOp ]
            [ longestDaysPastDueRadio mtEnabled (SetMoreThan "0") "přesahuje" "ldpd3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    Html.text "dnů"


numericInput : (String -> LongestDaysPastDueMsg) -> Bool -> String -> Html LongestDaysPastDueMsg
numericInput =
    View.NumericInput.numericInput 0 Constants.maxDaysDue


longestDaysPastDueRadio : Bool -> LongestDaysPastDueMsg -> String -> DomId -> Html LongestDaysPastDueMsg
longestDaysPastDueRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "longestDaysPastDue"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeLongestDaysPastDue : LongestDaysPastDue -> Value
encodeLongestDaysPastDue amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : LongestDaysPastDueCondition -> Value
encodeCondition (LongestDaysPastDueCondition c) =
    encodeLongestDaysPastDue c


longestDaysPastDueDecoder : Decoder LongestDaysPastDue
longestDaysPastDueDecoder =
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
                        Decode.fail <| "Unable to decode LongestDaysPastDue from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder LongestDaysPastDueCondition
conditionDecoder =
    Decode.map LongestDaysPastDueCondition longestDaysPastDueDecoder
