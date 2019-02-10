module Data.Filter.Conditions.TermMonths exposing
    ( TermMonths(..)
    , TermMonthsCondition(..)
    , TermMonthsMsg
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
import DomId exposing (DomId)
import Html exposing (Html, text)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (parseInt, zeroToEmpty)
import View.NumericInput


type TermMonths
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type TermMonthsCondition
    = TermMonthsCondition TermMonths


defaultCondition : TermMonthsCondition
defaultCondition =
    TermMonthsCondition (LessThan 0)


termMonthsToString : TermMonths -> String
termMonthsToString termMonths =
    case termMonths of
        LessThan maxBound ->
            "nedosahuje " ++ String.fromInt maxBound

        Between minBound maxBound ->
            "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound

        MoreThan minBound ->
            "přesahuje " ++ String.fromInt minBound


renderCondition : TermMonthsCondition -> String
renderCondition (TermMonthsCondition term) =
    "délka " ++ termMonthsToString term ++ " měsíců"


validationErrors : TermMonthsCondition -> List String
validationErrors (TermMonthsCondition t) =
    case t of
        LessThan x ->
            validateInRange 1 85 x

        Between x y ->
            validateInRange 0 84 x ++ validateInRange 0 84 y ++ validateMinNotGtMax x y

        MoreThan x ->
            validateInRange 0 83 x


validateInRange : Int -> Int -> Int -> List String
validateInRange minValid maxValid x =
    Util.validate (x < minValid || maxValid < x) <|
        "Délka úvěru v měsících: musí být v rozmezí "
            ++ String.fromInt minValid
            ++ " až "
            ++ String.fromInt maxValid


validateMinNotGtMax : Int -> Int -> List String
validateMinNotGtMax minBound maxBound =
    Util.validate (minBound > maxBound)
        "Délka úvěru v měsících: minimum nesmí být větší než maximum"


type TermMonthsMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | TermMonthsNoOp


whichEnabled : TermMonths -> ( Bool, Bool, Bool )
whichEnabled termMonths =
    case termMonths of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


update : TermMonthsMsg -> TermMonthsCondition -> TermMonthsCondition
update msg (TermMonthsCondition term) =
    TermMonthsCondition <|
        Maybe.withDefault term <|
            case msg of
                SetLessThan hi ->
                    Maybe.map LessThan (parseInt hi)

                SetBetween lo hi ->
                    Maybe.map2 Between (parseInt lo) (parseInt hi)

                SetMoreThan lo ->
                    Maybe.map MoreThan (parseInt lo)

                TermMonthsNoOp ->
                    Nothing


type alias TermMonthsRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : TermMonthsCondition -> Html TermMonthsMsg
form (TermMonthsCondition termMonths) =
    let
        values =
            case termMonths of
                LessThan x ->
                    TermMonthsRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    TermMonthsRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    TermMonthsRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled termMonths
    in
    Html.div []
        [ Form.formInline [ onSubmit TermMonthsNoOp ]
            [ termMonthsRadio ltEnabled (SetLessThan "0") "nedosahuje" "tm1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit TermMonthsNoOp ]
            [ termMonthsRadio btwEnabled (SetBetween "0" "0") "je" "tm2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit TermMonthsNoOp ]
            [ termMonthsRadio mtEnabled (SetMoreThan "0") "přesahuje" "tm3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    text "měsíců"


numericInput : (String -> TermMonthsMsg) -> Bool -> String -> Html TermMonthsMsg
numericInput =
    View.NumericInput.numericInput 0 85


termMonthsRadio : Bool -> TermMonthsMsg -> String -> DomId -> Html TermMonthsMsg
termMonthsRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "termMonths"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeTermMonths : TermMonths -> Value
encodeTermMonths amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : TermMonthsCondition -> Value
encodeCondition (TermMonthsCondition c) =
    encodeTermMonths c


termMonthsDecoder : Decoder TermMonths
termMonthsDecoder =
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
                        Decode.fail <| "Unable to decode TermMonths from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder TermMonthsCondition
conditionDecoder =
    Decode.map TermMonthsCondition termMonthsDecoder
