module Data.Filter.Conditions.TermMonths
    exposing
        ( TermMonths(..)
        , TermMonthsCondition(..)
        , TermMonthsMsg
        , conditionDecoder
        , defaultTermMonthsCondition
        , encodeCondition
        , renderTermMonthsCondition
        , termMonthsForm
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


type TermMonths
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type TermMonthsCondition
    = TermMonthsCondition TermMonths


defaultTermMonthsCondition : TermMonthsCondition
defaultTermMonthsCondition =
    TermMonthsCondition (MoreThan 0)


termMonthsToString : TermMonths -> String
termMonthsToString termMonths =
    case termMonths of
        LessThan maxBound ->
            "nedosahuje " ++ toString maxBound

        Between minBound maxBound ->
            "je " ++ toString minBound ++ " až " ++ toString maxBound

        MoreThan minBound ->
            "přesahuje " ++ toString minBound


renderTermMonthsCondition : TermMonthsCondition -> String
renderTermMonthsCondition (TermMonthsCondition term) =
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
    Util.validate (x < minValid || maxValid < x) <| "Délka úvěru: musí být v rozmezí " ++ toString minValid ++ " až " ++ toString maxValid


validateMinNotGtMax : Int -> Int -> List String
validateMinNotGtMax minBound maxBound =
    Util.validate (minBound > maxBound) "Délka úvěru: minimum nesmí být větší než maximum"


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
    case msg of
        SetLessThan hi ->
            emptyToZero hi |> String.toInt |> Result.map LessThan |> Result.withDefault term |> TermMonthsCondition

        SetBetween loStr hiStr ->
            emptyToZero loStr
                |> String.toInt
                |> Result.andThen (\lo -> emptyToZero hiStr |> String.toInt |> Result.map (\hi -> Between lo hi))
                |> Result.withDefault term
                |> TermMonthsCondition

        SetMoreThan lo ->
            emptyToZero lo |> String.toInt |> Result.map MoreThan |> Result.withDefault term |> TermMonthsCondition

        TermMonthsNoOp ->
            TermMonthsCondition term


termMonthsForm : TermMonthsCondition -> Html TermMonthsMsg
termMonthsForm (TermMonthsCondition termMonths) =
    let
        ( ltVal, btwMinVal, btwMaxVal, mtVal ) =
            case termMonths of
                LessThan x ->
                    ( zeroToEmpty x, "", "", "" )

                Between mi ma ->
                    ( "", zeroToEmpty mi, zeroToEmpty ma, "" )

                MoreThan x ->
                    ( "", "", "", zeroToEmpty x )

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled termMonths
    in
    Form.form [ onSubmit TermMonthsNoOp ]
        [ Form.formInline [ onSubmit TermMonthsNoOp ]
            [ termMonthsRadio ltEnabled (SetLessThan "0") "nedosahuje"
            , numericInput SetLessThan ltEnabled ltVal
            , text "měsíců"
            ]
        , Form.formInline [ onSubmit TermMonthsNoOp ]
            [ termMonthsRadio btwEnabled (SetBetween "0" "0") "je"
            , numericInput (\x -> SetBetween x btwMaxVal) btwEnabled btwMinVal
            , text "až"
            , numericInput (\y -> SetBetween btwMinVal y) btwEnabled btwMaxVal
            , text "měsíců"
            ]
        , Form.formInline [ onSubmit TermMonthsNoOp ]
            [ termMonthsRadio mtEnabled (SetMoreThan "0") "přesahuje"
            , numericInput SetMoreThan mtEnabled mtVal
            , text "měsíců"
            ]
        ]


numericInput : (String -> TermMonthsMsg) -> Bool -> String -> Html TermMonthsMsg
numericInput msg enabled value =
    Input.number
        [ Input.small
        , Input.onInput msg
        , Input.disabled <| not enabled
        , Input.value value
        , Input.attrs [ Attr.min "0", Attr.max "100", class "mx-1" ]
        ]


termMonthsRadio : Bool -> TermMonthsMsg -> String -> Html TermMonthsMsg
termMonthsRadio checked msg label =
    Radio.radio
        [ Radio.name "termMonths"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeTermMonths : TermMonths -> Value
encodeTermMonths amt =
    case amt of
        LessThan x ->
            Encode.list [ Encode.int 1, Encode.int x ]

        Between x y ->
            Encode.list [ Encode.int 2, Encode.int x, Encode.int y ]

        MoreThan y ->
            Encode.list [ Encode.int 3, Encode.int y ]


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
                        Decode.fail <| "Unable to decode TermMonths from " ++ toString ints
            )


conditionDecoder : Decoder TermMonthsCondition
conditionDecoder =
    Decode.map TermMonthsCondition termMonthsDecoder
