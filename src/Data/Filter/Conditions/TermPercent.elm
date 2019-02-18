module Data.Filter.Conditions.TermPercent exposing
    ( TermPercent(..)
    , TermPercentCondition(..)
    , TermPercentMsg
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
import Html exposing (Html, text)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (parseInt, zeroToEmpty)
import View.NumericInput


type TermPercent
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type TermPercentCondition
    = TermPercentCondition TermPercent


defaultCondition : TermPercentCondition
defaultCondition =
    TermPercentCondition (LessThan 0)


termPercentToString : TermPercent -> String
termPercentToString termPercent =
    case termPercent of
        LessThan maxBound ->
            "nedosahuje " ++ String.fromInt maxBound

        Between minBound maxBound ->
            "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound

        MoreThan minBound ->
            "přesahuje " ++ String.fromInt minBound


renderCondition : TermPercentCondition -> String
renderCondition (TermPercentCondition term) =
    "délka " ++ termPercentToString term ++ " % původní délky"


validationErrors : TermPercentCondition -> List String
validationErrors (TermPercentCondition t) =
    case t of
        LessThan x ->
            validateInRange 1 100 x

        Between x y ->
            validateInRange 0 99 x ++ validateInRange 1 100 y ++ minNotGtMax x y

        MoreThan x ->
            validateInRange 0 99 x


validateInRange : Int -> Int -> Int -> List String
validateInRange =
    Validate.intInRange "Délka úvěru v procentech"


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Délka úvěru v procentech"


type TermPercentMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | TermPercentNoOp


whichEnabled : TermPercent -> ( Bool, Bool, Bool )
whichEnabled termPercent =
    case termPercent of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


msgToModel : TermPercentMsg -> Maybe TermPercent
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        TermPercentNoOp ->
            Nothing


update : TermPercentMsg -> TermPercentCondition -> TermPercentCondition
update msg (TermPercentCondition term) =
    msgToModel msg
        |> Maybe.withDefault term
        |> TermPercentCondition



-- Elm 0.19: using this instead of 4-tuple


type alias TermPercentRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : TermPercentCondition -> Html TermPercentMsg
form (TermPercentCondition termPercent) =
    let
        values =
            case termPercent of
                LessThan x ->
                    TermPercentRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    TermPercentRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    TermPercentRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled termPercent
    in
    Html.div []
        [ Form.formInline [ onSubmit TermPercentNoOp ]
            [ termPercentRadio ltEnabled (SetLessThan "0") "nedosahuje"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit TermPercentNoOp ]
            [ termPercentRadio btwEnabled (SetBetween "0" "0") "je"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit TermPercentNoOp ]
            [ termPercentRadio mtEnabled (SetMoreThan "0") "přesahuje"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    text "% původní délky"


numericInput : (String -> TermPercentMsg) -> Bool -> String -> Html TermPercentMsg
numericInput =
    View.NumericInput.numericInput 0 100


termPercentRadio : Bool -> TermPercentMsg -> String -> Html TermPercentMsg
termPercentRadio checked msg label =
    Radio.radio
        [ Radio.name "termPercent"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeTermPercent : TermPercent -> Value
encodeTermPercent amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : TermPercentCondition -> Value
encodeCondition (TermPercentCondition c) =
    encodeTermPercent c


termPercentDecoder : Decoder TermPercent
termPercentDecoder =
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
                        Decode.fail <| "Unable to decode TermPercent from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder TermPercentCondition
conditionDecoder =
    Decode.map TermPercentCondition termPercentDecoder
