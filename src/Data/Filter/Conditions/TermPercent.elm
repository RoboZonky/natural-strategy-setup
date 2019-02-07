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
            validateInRange 0 99 x ++ validateInRange 1 100 y ++ validateMinNotGtMax x y

        MoreThan x ->
            validateInRange 0 99 x


validateInRange : Int -> Int -> Int -> List String
validateInRange minValid maxValid x =
    Util.validate (x < minValid || maxValid < x) <|
        "Délka úvěru v procentech: musí být v rozmezí "
            ++ String.fromInt minValid
            ++ " až "
            ++ String.fromInt maxValid


validateMinNotGtMax : Int -> Int -> List String
validateMinNotGtMax minBound maxBound =
    Util.validate (minBound > maxBound) "Délka úvěru v procentech: minimum nesmí být větší než maximum"


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


update : TermPercentMsg -> TermPercentCondition -> TermPercentCondition
update msg (TermPercentCondition term) =
    TermPercentCondition <|
        Maybe.withDefault term <|
            case msg of
                SetLessThan hi ->
                    Maybe.map LessThan (parseInt hi)

                SetBetween lo hi ->
                    Maybe.map2 Between (parseInt lo) (parseInt hi)

                SetMoreThan lo ->
                    Maybe.map MoreThan (parseInt lo)

                TermPercentNoOp ->
                    Nothing



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
            , text "% původní délky"
            ]
        , Form.formInline [ onSubmit TermPercentNoOp ]
            [ termPercentRadio btwEnabled (SetBetween "0" "0") "je"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , text "% původní délky"
            ]
        , Form.formInline [ onSubmit TermPercentNoOp ]
            [ termPercentRadio mtEnabled (SetMoreThan "0") "přesahuje"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , text "% původní délky"
            ]
        ]


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
