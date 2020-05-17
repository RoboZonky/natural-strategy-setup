module Data.Filter.Conditions.LoanAnnuity exposing
    ( LoanAnnuity(..)
    , LoanAnnuityCondition(..)
    , LoanAnnuityMsg
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
import Data.Validate as Validate
import DomId exposing (DomId)
import Html exposing (Html)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (parseInt, zeroToEmpty)
import View.NumericInput


type LoanAnnuity
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type LoanAnnuityCondition
    = LoanAnnuityCondition LoanAnnuity


defaultCondition : LoanAnnuityCondition
defaultCondition =
    LoanAnnuityCondition (MoreThan 0)


renderCondition : LoanAnnuityCondition -> String
renderCondition (LoanAnnuityCondition loanAnnuity) =
    "měsíční splátka  " ++ loanAnnuityToString loanAnnuity ++ " Kč"


loanAnnuityToString : LoanAnnuity -> String
loanAnnuityToString revenueRate =
    case revenueRate of
        Between from to ->
            "je " ++ String.fromInt from ++ " až " ++ String.fromInt to

        MoreThan lowerBound ->
            "přesahuje " ++ String.fromInt lowerBound

        LessThan upperBound ->
            "nedosahuje " ++ String.fromInt upperBound


validationErrors : LoanAnnuityCondition -> List String
validationErrors (LoanAnnuityCondition a) =
    case a of
        LessThan x ->
            positive x

        Between x y ->
            positive x ++ positive y ++ minNotGtMax x y

        MoreThan x ->
            positive x


positive : Int -> List String
positive =
    Validate.positive "Měsíční splátka"


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Měsíční splátka"


type LoanAnnuityMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | LoanAnnuityNoOp


msgToModel : LoanAnnuityMsg -> Maybe LoanAnnuity
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        LoanAnnuityNoOp ->
            Nothing


update : LoanAnnuityMsg -> LoanAnnuityCondition -> LoanAnnuityCondition
update msg (LoanAnnuityCondition la) =
    msgToModel msg
        |> Maybe.withDefault la
        |> LoanAnnuityCondition


type alias LoanAnnuityRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : LoanAnnuityCondition -> Html LoanAnnuityMsg
form (LoanAnnuityCondition la) =
    let
        values =
            case la of
                LessThan x ->
                    LoanAnnuityRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    LoanAnnuityRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    LoanAnnuityRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled la
    in
    Html.div []
        [ Form.formInline [ onSubmit LoanAnnuityNoOp ]
            [ loanAnnuityRadio ltEnabled (SetLessThan "0") "nedosahuje" "annuity1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit LoanAnnuityNoOp ]
            [ loanAnnuityRadio btwEnabled (SetBetween "0" "0") "je" "annuity2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , Html.text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit LoanAnnuityNoOp ]
            [ loanAnnuityRadio mtEnabled (SetMoreThan "0") "přesahuje" "annuity3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    Html.text "Kč"


numericInput : (String -> LoanAnnuityMsg) -> Bool -> String -> Html LoanAnnuityMsg
numericInput =
    View.NumericInput.numericInput 0 1000000


loanAnnuityRadio : Bool -> LoanAnnuityMsg -> String -> DomId -> Html LoanAnnuityMsg
loanAnnuityRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "loanAnnuity"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label


whichEnabled : LoanAnnuity -> ( Bool, Bool, Bool )
whichEnabled amt =
    case amt of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )



-- JSON


encodeLoanAnnuity : LoanAnnuity -> Value
encodeLoanAnnuity la =
    case la of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : LoanAnnuityCondition -> Value
encodeCondition (LoanAnnuityCondition c) =
    encodeLoanAnnuity c


loanAnnuityDecoder : Decoder LoanAnnuity
loanAnnuityDecoder =
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
                        Decode.fail <| "Unable to decode LoanAnuity from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder LoanAnnuityCondition
conditionDecoder =
    Decode.map LoanAnnuityCondition loanAnnuityDecoder
