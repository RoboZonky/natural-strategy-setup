module Data.Filter.Conditions.RemainingTermMonths exposing
    ( RemainingTermMonths(..)
    , RemainingTermMonthsCondition(..)
    , RemainingTermMonthsMsg
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


type RemainingTermMonths
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type RemainingTermMonthsCondition
    = RemainingTermMonthsCondition RemainingTermMonths


defaultCondition : RemainingTermMonthsCondition
defaultCondition =
    RemainingTermMonthsCondition (LessThan 0)


remainingTermMonthsToString : RemainingTermMonths -> String
remainingTermMonthsToString termMonths =
    case termMonths of
        LessThan maxBound ->
            "nedosahuje " ++ String.fromInt maxBound

        Between minBound maxBound ->
            "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound

        MoreThan minBound ->
            "přesahuje " ++ String.fromInt minBound


renderCondition : RemainingTermMonthsCondition -> String
renderCondition (RemainingTermMonthsCondition term) =
    "délka " ++ remainingTermMonthsToString term ++ " měsíců"


validationErrors : RemainingTermMonthsCondition -> List String
validationErrors (RemainingTermMonthsCondition t) =
    case t of
        LessThan x ->
            validateInRange (minTermMonths + 1) (maxTermMonths + 1) x

        Between x y ->
            validateInRange minTermMonths maxTermMonths x ++ validateInRange minTermMonths maxTermMonths y ++ minNotGtMax x y

        MoreThan x ->
            validateInRange minTermMonths (maxTermMonths - 1) x


validateInRange : Int -> Int -> Int -> List String
validateInRange =
    Validate.intInRange "Zbývající délka půjčky v měsících"


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Zbývající délka půjčky v měsících"


type RemainingTermMonthsMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | TermMonthsNoOp


whichEnabled : RemainingTermMonths -> ( Bool, Bool, Bool )
whichEnabled termMonths =
    case termMonths of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


msgToModel : RemainingTermMonthsMsg -> Maybe RemainingTermMonths
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        TermMonthsNoOp ->
            Nothing


update : RemainingTermMonthsMsg -> RemainingTermMonthsCondition -> RemainingTermMonthsCondition
update msg (RemainingTermMonthsCondition term) =
    msgToModel msg
        |> Maybe.withDefault term
        |> RemainingTermMonthsCondition


type alias RemainingTermMonthsRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : RemainingTermMonthsCondition -> Html RemainingTermMonthsMsg
form (RemainingTermMonthsCondition termMonths) =
    let
        values =
            case termMonths of
                LessThan x ->
                    RemainingTermMonthsRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    RemainingTermMonthsRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    RemainingTermMonthsRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled termMonths
    in
    Html.div []
        [ Form.formInline [ onSubmit TermMonthsNoOp ]
            [ remainingTermMonthsRadio ltEnabled (SetLessThan "0") "nedosahuje" "rtm1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit TermMonthsNoOp ]
            [ remainingTermMonthsRadio btwEnabled (SetBetween "0" "0") "je" "rtm2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , Html.text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit TermMonthsNoOp ]
            [ remainingTermMonthsRadio mtEnabled (SetMoreThan "0") "přesahuje" "rtm3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    Html.text "měsíců"


numericInput : (String -> RemainingTermMonthsMsg) -> Bool -> String -> Html RemainingTermMonthsMsg
numericInput =
    View.NumericInput.numericInput 0 85


remainingTermMonthsRadio : Bool -> RemainingTermMonthsMsg -> String -> DomId -> Html RemainingTermMonthsMsg
remainingTermMonthsRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "remainingTermMonths"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeRemainingTermMonths : RemainingTermMonths -> Value
encodeRemainingTermMonths amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : RemainingTermMonthsCondition -> Value
encodeCondition (RemainingTermMonthsCondition c) =
    encodeRemainingTermMonths c


remainingTermMonthsDecoder : Decoder RemainingTermMonths
remainingTermMonthsDecoder =
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
                        Decode.fail <| "Unable to decode RemainingTermMonths from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder RemainingTermMonthsCondition
conditionDecoder =
    Decode.map RemainingTermMonthsCondition remainingTermMonthsDecoder
