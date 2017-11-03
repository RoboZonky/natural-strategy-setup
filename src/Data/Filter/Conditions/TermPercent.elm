module Data.Filter.Conditions.TermPercent
    exposing
        ( TermPercent(..)
        , TermPercentCondition(..)
        , TermPercentMsg
        , conditionDecoder
        , defaultTermPercentCondition
        , encodeCondition
        , renderTermPercentCondition
        , termPercentForm
        , update
        , validationErrors
        )

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Html exposing (Html, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (emptyToZero, zeroToEmpty)


type TermPercent
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type TermPercentCondition
    = TermPercentCondition TermPercent


defaultTermPercentCondition : TermPercentCondition
defaultTermPercentCondition =
    TermPercentCondition (MoreThan 0)


termPercentToString : TermPercent -> String
termPercentToString termPercent =
    case termPercent of
        LessThan maxBound ->
            "nedosahuje " ++ toString maxBound

        Between minBound maxBound ->
            "je " ++ toString minBound ++ " až " ++ toString maxBound

        MoreThan minBound ->
            "přesahuje " ++ toString minBound


renderTermPercentCondition : TermPercentCondition -> String
renderTermPercentCondition (TermPercentCondition term) =
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
    Util.validate (x < minValid || maxValid < x) <| "Délka úvěru v procentech: musí být v rozmezí " ++ toString minValid ++ " až " ++ toString maxValid


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
    case msg of
        SetLessThan hi ->
            emptyToZero hi |> String.toInt |> Result.map LessThan |> Result.withDefault term |> TermPercentCondition

        SetBetween loStr hiStr ->
            emptyToZero loStr
                |> String.toInt
                |> Result.andThen (\lo -> emptyToZero hiStr |> String.toInt |> Result.map (\hi -> Between lo hi))
                |> Result.withDefault term
                |> TermPercentCondition

        SetMoreThan lo ->
            emptyToZero lo |> String.toInt |> Result.map MoreThan |> Result.withDefault term |> TermPercentCondition

        TermPercentNoOp ->
            TermPercentCondition term


termPercentForm : TermPercentCondition -> Html TermPercentMsg
termPercentForm (TermPercentCondition termPercent) =
    let
        ( ltVal, btwMinVal, btwMaxVal, mtVal ) =
            case termPercent of
                LessThan x ->
                    ( zeroToEmpty x, "", "", "" )

                Between mi ma ->
                    ( "", zeroToEmpty mi, zeroToEmpty ma, "" )

                MoreThan x ->
                    ( "", "", "", zeroToEmpty x )

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled termPercent
    in
    Form.form [ onSubmit TermPercentNoOp ]
        [ Form.formInline [ onSubmit TermPercentNoOp ]
            [ termPercentRadio ltEnabled (SetLessThan "0") "nedosahuje"
            , numericInput SetLessThan ltEnabled ltVal
            , text "% původní délky"
            ]
        , Form.formInline [ onSubmit TermPercentNoOp ]
            [ termPercentRadio btwEnabled (SetBetween "0" "0") "je"
            , numericInput (\x -> SetBetween x btwMaxVal) btwEnabled btwMinVal
            , text "až"
            , numericInput (\y -> SetBetween btwMinVal y) btwEnabled btwMaxVal
            , text "% původní délky"
            ]
        , Form.formInline [ onSubmit TermPercentNoOp ]
            [ termPercentRadio mtEnabled (SetMoreThan "0") "přesahuje"
            , numericInput SetMoreThan mtEnabled mtVal
            , text "% původní délky"
            ]
        ]


numericInput : (String -> TermPercentMsg) -> Bool -> String -> Html TermPercentMsg
numericInput msg enabled value =
    Input.number
        [ Input.small
        , Input.onInput msg
        , Input.disabled <| not enabled
        , Input.value value
        , Input.attrs [ Attr.min "0", Attr.max "100", class "mx-1" ]
        ]


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
            Encode.list [ Encode.int 1, Encode.int x ]

        Between x y ->
            Encode.list [ Encode.int 2, Encode.int x, Encode.int y ]

        MoreThan y ->
            Encode.list [ Encode.int 3, Encode.int y ]


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
                        Decode.fail <| "Unable to decode TermPercent from " ++ toString ints
            )


conditionDecoder : Decoder TermPercentCondition
conditionDecoder =
    Decode.map TermPercentCondition termPercentDecoder
