module Data.Filter.Conditions.ElapsedTermMonths exposing
    ( ElapsedTermMonths(..)
    , ElapsedTermMonthsCondition(..)
    , ElapsedTermMonthsMsg
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
import Data.Filter.Constants exposing (maxTermMonths, minTermMonths)
import Data.Validate as Validate
import DomId exposing (DomId)
import Html exposing (Html)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (parseInt, zeroToEmpty)
import View.NumericInput


type ElapsedTermMonths
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type ElapsedTermMonthsCondition
    = ElapsedTermMonthsCondition ElapsedTermMonths


defaultCondition : ElapsedTermMonthsCondition
defaultCondition =
    ElapsedTermMonthsCondition (LessThan 0)


elapsedTermMonthsToString : ElapsedTermMonths -> String
elapsedTermMonthsToString elapsedTermMonths =
    case elapsedTermMonths of
        LessThan maxBound ->
            "méně než " ++ String.fromInt maxBound

        Between minBound maxBound ->
            "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound

        MoreThan minBound ->
            "více než " ++ String.fromInt minBound


renderCondition : ElapsedTermMonthsCondition -> String
renderCondition (ElapsedTermMonthsCondition term) =
    "uhrazeno " ++ elapsedTermMonthsToString term ++ " splátek"


validationErrors : ElapsedTermMonthsCondition -> List String
validationErrors (ElapsedTermMonthsCondition t) =
    case t of
        LessThan x ->
            validateInRange (minTermMonths + 1) (maxTermMonths + 1) x

        Between x y ->
            validateInRange minTermMonths maxTermMonths x ++ validateInRange minTermMonths maxTermMonths y ++ minNotGtMax x y

        MoreThan x ->
            validateInRange minTermMonths (maxTermMonths - 1) x


validateInRange : Int -> Int -> Int -> List String
validateInRange =
    Validate.intInRange "Počet uhrazených splátek v měsících"


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Počet uhrazených splátek"


type ElapsedTermMonthsMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | ElapsedTermMonthsNoOp


whichEnabled : ElapsedTermMonths -> ( Bool, Bool, Bool )
whichEnabled elapsedTermMonths =
    case elapsedTermMonths of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


msgToModel : ElapsedTermMonthsMsg -> Maybe ElapsedTermMonths
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        ElapsedTermMonthsNoOp ->
            Nothing


update : ElapsedTermMonthsMsg -> ElapsedTermMonthsCondition -> ElapsedTermMonthsCondition
update msg (ElapsedTermMonthsCondition term) =
    msgToModel msg
        |> Maybe.withDefault term
        |> ElapsedTermMonthsCondition


type alias ElapsedTermMonthsRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : ElapsedTermMonthsCondition -> Html ElapsedTermMonthsMsg
form (ElapsedTermMonthsCondition elapsedTermMonths) =
    let
        values =
            case elapsedTermMonths of
                LessThan x ->
                    ElapsedTermMonthsRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    ElapsedTermMonthsRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    ElapsedTermMonthsRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled elapsedTermMonths
    in
    Html.div []
        [ Form.formInline [ onSubmit ElapsedTermMonthsNoOp ]
            [ elapsedTermMonthsRadio ltEnabled (SetLessThan "0") "méně než" "etm1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit ElapsedTermMonthsNoOp ]
            [ elapsedTermMonthsRadio btwEnabled (SetBetween "0" "0") "je" "etm2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , Html.text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit ElapsedTermMonthsNoOp ]
            [ elapsedTermMonthsRadio mtEnabled (SetMoreThan "0") "více než" "etm3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    Html.text "splátek"


numericInput : (String -> ElapsedTermMonthsMsg) -> Bool -> String -> Html ElapsedTermMonthsMsg
numericInput =
    View.NumericInput.numericInput 0 85


elapsedTermMonthsRadio : Bool -> ElapsedTermMonthsMsg -> String -> DomId -> Html ElapsedTermMonthsMsg
elapsedTermMonthsRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "elapsedTermMonths"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeElapsedTermMonths : ElapsedTermMonths -> Value
encodeElapsedTermMonths amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : ElapsedTermMonthsCondition -> Value
encodeCondition (ElapsedTermMonthsCondition c) =
    encodeElapsedTermMonths c


elapsedTermMonthsDecoder : Decoder ElapsedTermMonths
elapsedTermMonthsDecoder =
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
                        Decode.fail <| "Unable to decode ElapsedTermMonths from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder ElapsedTermMonthsCondition
conditionDecoder =
    Decode.map ElapsedTermMonthsCondition elapsedTermMonthsDecoder
