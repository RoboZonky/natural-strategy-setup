module Data.Filter.Conditions.ElapsedTermMonths
    exposing
        ( ElapsedTermMonths(Between, LessThan, MoreThan)
        , ElapsedTermMonthsCondition(ElapsedTermMonthsCondition)
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
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import DomId exposing (DomId)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (emptyToZero, zeroToEmpty)


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
            "méně než " ++ toString maxBound

        Between minBound maxBound ->
            "je " ++ toString minBound ++ " až " ++ toString maxBound

        MoreThan minBound ->
            "více než " ++ toString minBound


renderCondition : ElapsedTermMonthsCondition -> String
renderCondition (ElapsedTermMonthsCondition term) =
    "uhrazeno " ++ elapsedTermMonthsToString term ++ " splátek"


validationErrors : ElapsedTermMonthsCondition -> List String
validationErrors (ElapsedTermMonthsCondition t) =
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
        "Počet uhrazených splátek v měsících musí být v rozmezí "
            ++ toString minValid
            ++ " až "
            ++ toString maxValid


validateMinNotGtMax : Int -> Int -> List String
validateMinNotGtMax minBound maxBound =
    Util.validate (minBound > maxBound) "Počet uhrazených splátek: minimum nesmí být větší než maximum"


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


update : ElapsedTermMonthsMsg -> ElapsedTermMonthsCondition -> ElapsedTermMonthsCondition
update msg (ElapsedTermMonthsCondition term) =
    case msg of
        SetLessThan hi ->
            emptyToZero hi |> String.toInt |> Result.map LessThan |> Result.withDefault term |> ElapsedTermMonthsCondition

        SetBetween loStr hiStr ->
            emptyToZero loStr
                |> String.toInt
                |> Result.andThen (\lo -> emptyToZero hiStr |> String.toInt |> Result.map (\hi -> Between lo hi))
                |> Result.withDefault term
                |> ElapsedTermMonthsCondition

        SetMoreThan lo ->
            emptyToZero lo |> String.toInt |> Result.map MoreThan |> Result.withDefault term |> ElapsedTermMonthsCondition

        ElapsedTermMonthsNoOp ->
            ElapsedTermMonthsCondition term


form : ElapsedTermMonthsCondition -> Html ElapsedTermMonthsMsg
form (ElapsedTermMonthsCondition elapsedTermMonths) =
    let
        ( ltVal, btwMinVal, btwMaxVal, mtVal ) =
            case elapsedTermMonths of
                LessThan x ->
                    ( zeroToEmpty x, "", "", "" )

                Between mi ma ->
                    ( "", zeroToEmpty mi, zeroToEmpty ma, "" )

                MoreThan x ->
                    ( "", "", "", zeroToEmpty x )

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled elapsedTermMonths
    in
    Html.div []
        [ Form.formInline [ onSubmit ElapsedTermMonthsNoOp ]
            [ elapsedTermMonthsRadio ltEnabled (SetLessThan "0") "méně než" "etm1"
            , numericInput SetLessThan ltEnabled ltVal
            , text "splátek"
            ]
        , Form.formInline [ onSubmit ElapsedTermMonthsNoOp ]
            [ elapsedTermMonthsRadio btwEnabled (SetBetween "0" "0") "je" "etm2"
            , numericInput (\x -> SetBetween x btwMaxVal) btwEnabled btwMinVal
            , text "až"
            , numericInput (\y -> SetBetween btwMinVal y) btwEnabled btwMaxVal
            , text "splátek"
            ]
        , Form.formInline [ onSubmit ElapsedTermMonthsNoOp ]
            [ elapsedTermMonthsRadio mtEnabled (SetMoreThan "0") "více než" "etm3"
            , numericInput SetMoreThan mtEnabled mtVal
            , text "splátek"
            ]
        ]


numericInput : (String -> ElapsedTermMonthsMsg) -> Bool -> String -> Html ElapsedTermMonthsMsg
numericInput msg enabled value =
    Input.number
        [ Input.small
        , Input.onInput msg
        , Input.disabled <| not enabled
        , Input.value value
        , Input.attrs [ Attr.min "0", Attr.max "85", Spacing.mx1 ]
        ]


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
            Encode.list [ Encode.int 1, Encode.int x ]

        Between x y ->
            Encode.list [ Encode.int 2, Encode.int x, Encode.int y ]

        MoreThan y ->
            Encode.list [ Encode.int 3, Encode.int y ]


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
                        Decode.fail <| "Unable to decode ElapsedTermMonths from " ++ toString ints
            )


conditionDecoder : Decoder ElapsedTermMonthsCondition
conditionDecoder =
    Decode.map ElapsedTermMonthsCondition elapsedTermMonthsDecoder
