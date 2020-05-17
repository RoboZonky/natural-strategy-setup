module Data.Filter.Conditions.OriginalTermMonths exposing
    ( OriginalTermMonths(..)
    , OriginalTermMonthsCondition(..)
    , OriginalTermMonthsMsg
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


type OriginalTermMonths
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type OriginalTermMonthsCondition
    = OriginalTermMonthsCondition OriginalTermMonths


defaultCondition : OriginalTermMonthsCondition
defaultCondition =
    OriginalTermMonthsCondition (LessThan 0)


originalTermMonthsToString : OriginalTermMonths -> String
originalTermMonthsToString termMonths =
    case termMonths of
        LessThan maxBound ->
            "nedosahuje " ++ String.fromInt maxBound

        Between minBound maxBound ->
            "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound

        MoreThan minBound ->
            "přesahuje " ++ String.fromInt minBound


renderCondition : OriginalTermMonthsCondition -> String
renderCondition (OriginalTermMonthsCondition term) =
    "původní délka " ++ originalTermMonthsToString term ++ " měsíců"


validationErrors : OriginalTermMonthsCondition -> List String
validationErrors (OriginalTermMonthsCondition t) =
    case t of
        LessThan x ->
            validateInRange 1 85 x

        Between x y ->
            validateInRange 0 84 x ++ validateInRange 0 84 y ++ minNotGtMax x y

        MoreThan x ->
            validateInRange 0 83 x


validateInRange : Int -> Int -> Int -> List String
validateInRange =
    Validate.intInRange "Původní délka úvěru v měsících"


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Původní délka úvěru v měsících"


type OriginalTermMonthsMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | TermMonthsNoOp


whichEnabled : OriginalTermMonths -> ( Bool, Bool, Bool )
whichEnabled termMonths =
    case termMonths of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


msgToModel : OriginalTermMonthsMsg -> Maybe OriginalTermMonths
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


update : OriginalTermMonthsMsg -> OriginalTermMonthsCondition -> OriginalTermMonthsCondition
update msg (OriginalTermMonthsCondition term) =
    msgToModel msg
        |> Maybe.withDefault term
        |> OriginalTermMonthsCondition


type alias OriginalTermMonthsRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : OriginalTermMonthsCondition -> Html OriginalTermMonthsMsg
form (OriginalTermMonthsCondition termMonths) =
    let
        values =
            case termMonths of
                LessThan x ->
                    OriginalTermMonthsRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    OriginalTermMonthsRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    OriginalTermMonthsRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled termMonths
    in
    Html.div []
        [ Form.formInline [ onSubmit TermMonthsNoOp ]
            [ originalTermMonthsRadio ltEnabled (SetLessThan "0") "nedosahuje" "otm1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit TermMonthsNoOp ]
            [ originalTermMonthsRadio btwEnabled (SetBetween "0" "0") "je" "otm2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , Html.text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit TermMonthsNoOp ]
            [ originalTermMonthsRadio mtEnabled (SetMoreThan "0") "přesahuje" "otm3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    Html.text "měsíců"


numericInput : (String -> OriginalTermMonthsMsg) -> Bool -> String -> Html OriginalTermMonthsMsg
numericInput =
    View.NumericInput.numericInput 0 85


originalTermMonthsRadio : Bool -> OriginalTermMonthsMsg -> String -> DomId -> Html OriginalTermMonthsMsg
originalTermMonthsRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "originalTermMonths"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeOriginalTermMonths : OriginalTermMonths -> Value
encodeOriginalTermMonths amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : OriginalTermMonthsCondition -> Value
encodeCondition (OriginalTermMonthsCondition c) =
    encodeOriginalTermMonths c


originalTermMonthsDecoder : Decoder OriginalTermMonths
originalTermMonthsDecoder =
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
                        Decode.fail <| "Unable to decode OriginalTermMonths from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder OriginalTermMonthsCondition
conditionDecoder =
    Decode.map OriginalTermMonthsCondition originalTermMonthsDecoder
