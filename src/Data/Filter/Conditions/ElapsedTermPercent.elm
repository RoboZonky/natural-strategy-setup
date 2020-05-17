module Data.Filter.Conditions.ElapsedTermPercent exposing
    ( ElapsedTermPercent(..)
    , ElapsedTermPercentCondition(..)
    , ElapsedTermPercentMsg
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


type ElapsedTermPercent
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type ElapsedTermPercentCondition
    = ElapsedTermPercentCondition ElapsedTermPercent


defaultCondition : ElapsedTermPercentCondition
defaultCondition =
    ElapsedTermPercentCondition (LessThan 0)


elapsedTermPercentToString : ElapsedTermPercent -> String
elapsedTermPercentToString elapsedTermPercent =
    case elapsedTermPercent of
        LessThan maxBound ->
            "méně než " ++ String.fromInt maxBound

        Between minBound maxBound ->
            "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound

        MoreThan minBound ->
            "více než " ++ String.fromInt minBound


renderCondition : ElapsedTermPercentCondition -> String
renderCondition (ElapsedTermPercentCondition term) =
    "uhrazeno " ++ elapsedTermPercentToString term ++ " % splátek"


validationErrors : ElapsedTermPercentCondition -> List String
validationErrors (ElapsedTermPercentCondition t) =
    case t of
        LessThan x ->
            validateInRange 1 100 x

        Between x y ->
            validateInRange 0 99 x ++ validateInRange 1 100 y ++ minNotGtMax x y

        MoreThan x ->
            validateInRange 0 99 x


validateInRange : Int -> Int -> Int -> List String
validateInRange =
    Validate.intInRange "Počet uhrazených splátek v procentech"


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Počet uhrazených splátek v procentech"


type ElapsedTermPercentMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | ElapsedTermPercentNoOp


whichEnabled : ElapsedTermPercent -> ( Bool, Bool, Bool )
whichEnabled elapsedTermPercent =
    case elapsedTermPercent of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


msgToModel : ElapsedTermPercentMsg -> Maybe ElapsedTermPercent
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        ElapsedTermPercentNoOp ->
            Nothing


update : ElapsedTermPercentMsg -> ElapsedTermPercentCondition -> ElapsedTermPercentCondition
update msg (ElapsedTermPercentCondition term) =
    msgToModel msg
        |> Maybe.withDefault term
        |> ElapsedTermPercentCondition


type alias ElapsedTermPercentRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : ElapsedTermPercentCondition -> Html ElapsedTermPercentMsg
form (ElapsedTermPercentCondition elapsedTermPercent) =
    let
        values =
            case elapsedTermPercent of
                LessThan x ->
                    ElapsedTermPercentRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    ElapsedTermPercentRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    ElapsedTermPercentRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled elapsedTermPercent
    in
    Html.div []
        [ Form.formInline [ onSubmit ElapsedTermPercentNoOp ]
            [ elapsedTermPercentRadio ltEnabled (SetLessThan "0") "méně než" "etp1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit ElapsedTermPercentNoOp ]
            [ elapsedTermPercentRadio btwEnabled (SetBetween "0" "0") "je" "etp2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , Html.text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit ElapsedTermPercentNoOp ]
            [ elapsedTermPercentRadio mtEnabled (SetMoreThan "0") "více než" "etp3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    Html.text "% splátek"


numericInput : (String -> ElapsedTermPercentMsg) -> Bool -> String -> Html ElapsedTermPercentMsg
numericInput =
    View.NumericInput.numericInput 0 100


elapsedTermPercentRadio : Bool -> ElapsedTermPercentMsg -> String -> DomId -> Html ElapsedTermPercentMsg
elapsedTermPercentRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "elapsedTermPercent"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeElapsedTermPercent : ElapsedTermPercent -> Value
encodeElapsedTermPercent amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : ElapsedTermPercentCondition -> Value
encodeCondition (ElapsedTermPercentCondition c) =
    encodeElapsedTermPercent c


elapsedTermPercentDecoder : Decoder ElapsedTermPercent
elapsedTermPercentDecoder =
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
                        Decode.fail <| "Unable to decode ElapsedTermPercent from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder ElapsedTermPercentCondition
conditionDecoder =
    Decode.map ElapsedTermPercentCondition elapsedTermPercentDecoder
