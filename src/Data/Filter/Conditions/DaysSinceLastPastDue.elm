module Data.Filter.Conditions.DaysSinceLastPastDue exposing
    ( DaysSinceLastPastDue(..)
    , DaysSinceLastPastDueCondition(..)
    , DaysSinceLastPastDueMsg
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


type DaysSinceLastPastDue
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type DaysSinceLastPastDueCondition
    = DaysSinceLastPastDueCondition DaysSinceLastPastDue


defaultCondition : DaysSinceLastPastDueCondition
defaultCondition =
    DaysSinceLastPastDueCondition (LessThan 0)


daysSinceLastPastDueToString : DaysSinceLastPastDue -> String
daysSinceLastPastDueToString daysSinceLastPastDue =
    case daysSinceLastPastDue of
        LessThan maxBound ->
            "nedosahuje " ++ String.fromInt maxBound

        Between minBound maxBound ->
            "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound

        MoreThan minBound ->
            "přesahuje " ++ String.fromInt minBound


renderCondition : DaysSinceLastPastDueCondition -> String
renderCondition (DaysSinceLastPastDueCondition condition) =
    "doba od posledního dne po splatnosti " ++ daysSinceLastPastDueToString condition ++ " dnů"


validationErrors : DaysSinceLastPastDueCondition -> List String
validationErrors (DaysSinceLastPastDueCondition t) =
    case t of
        LessThan x ->
            validateInRange x

        Between x y ->
            validateInRange x ++ validateInRange y ++ minNotGtMax x y

        MoreThan x ->
            validateInRange x


validateInRange : Int -> List String
validateInRange =
    Validate.intInRange "Doba od posledního dne po splatnosti ve dnech" 0 Constants.maxDaysDue


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Doba od posledního dne po splatnosti ve dnech"


type DaysSinceLastPastDueMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | DaysSinceLastPastDueNoOp


whichEnabled : DaysSinceLastPastDue -> ( Bool, Bool, Bool )
whichEnabled daysSinceLastPastDue =
    case daysSinceLastPastDue of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


msgToModel : DaysSinceLastPastDueMsg -> Maybe DaysSinceLastPastDue
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        DaysSinceLastPastDueNoOp ->
            Nothing


update : DaysSinceLastPastDueMsg -> DaysSinceLastPastDueCondition -> DaysSinceLastPastDueCondition
update msg (DaysSinceLastPastDueCondition cond) =
    msgToModel msg
        |> Maybe.withDefault cond
        |> DaysSinceLastPastDueCondition


type alias DaysSinceLastPastDueRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : DaysSinceLastPastDueCondition -> Html DaysSinceLastPastDueMsg
form (DaysSinceLastPastDueCondition condition) =
    let
        values =
            case condition of
                LessThan x ->
                    DaysSinceLastPastDueRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    DaysSinceLastPastDueRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    DaysSinceLastPastDueRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled condition
    in
    Html.div []
        [ Form.formInline [ onSubmit DaysSinceLastPastDueNoOp ]
            [ daysSinceLastPastDueRadio ltEnabled (SetLessThan "0") "nedosahuje" "dslpd1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit DaysSinceLastPastDueNoOp ]
            [ daysSinceLastPastDueRadio btwEnabled (SetBetween "0" "0") "je" "dslpd2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , Html.text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit DaysSinceLastPastDueNoOp ]
            [ daysSinceLastPastDueRadio mtEnabled (SetMoreThan "0") "přesahuje" "dslpd3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    Html.text "dnů"


numericInput : (String -> DaysSinceLastPastDueMsg) -> Bool -> String -> Html DaysSinceLastPastDueMsg
numericInput =
    View.NumericInput.numericInput 0 Constants.maxDaysDue


daysSinceLastPastDueRadio : Bool -> DaysSinceLastPastDueMsg -> String -> DomId -> Html DaysSinceLastPastDueMsg
daysSinceLastPastDueRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "daysSinceLastPastDue"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeDaysSinceLastPastDue : DaysSinceLastPastDue -> Value
encodeDaysSinceLastPastDue amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : DaysSinceLastPastDueCondition -> Value
encodeCondition (DaysSinceLastPastDueCondition c) =
    encodeDaysSinceLastPastDue c


daysSinceLastPastDueDecoder : Decoder DaysSinceLastPastDue
daysSinceLastPastDueDecoder =
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
                        Decode.fail <| "Unable to decode DaysSinceLastPastDue from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder DaysSinceLastPastDueCondition
conditionDecoder =
    Decode.map DaysSinceLastPastDueCondition daysSinceLastPastDueDecoder
