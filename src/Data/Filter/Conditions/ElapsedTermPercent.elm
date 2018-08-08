module Data.Filter.Conditions.ElapsedTermPercent
    exposing
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
import DomId exposing (DomId)
import Html exposing (Html, text)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (emptyToZero, zeroToEmpty)
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
            "méně než " ++ toString maxBound

        Between minBound maxBound ->
            "je " ++ toString minBound ++ " až " ++ toString maxBound

        MoreThan minBound ->
            "více než " ++ toString minBound


renderCondition : ElapsedTermPercentCondition -> String
renderCondition (ElapsedTermPercentCondition term) =
    "uhrazeno " ++ elapsedTermPercentToString term ++ " % splátek"


validationErrors : ElapsedTermPercentCondition -> List String
validationErrors (ElapsedTermPercentCondition t) =
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
        "Počet uhrazených splátek v procentech musí být v rozmezí "
            ++ toString minValid
            ++ " až "
            ++ toString maxValid


validateMinNotGtMax : Int -> Int -> List String
validateMinNotGtMax minBound maxBound =
    Util.validate (minBound > maxBound) "Počet uhrazených splátek v procentech: minimum nesmí být větší než maximum"


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


update : ElapsedTermPercentMsg -> ElapsedTermPercentCondition -> ElapsedTermPercentCondition
update msg (ElapsedTermPercentCondition term) =
    case msg of
        SetLessThan hi ->
            emptyToZero hi |> String.toInt |> Result.map LessThan |> Result.withDefault term |> ElapsedTermPercentCondition

        SetBetween loStr hiStr ->
            emptyToZero loStr
                |> String.toInt
                |> Result.andThen (\lo -> emptyToZero hiStr |> String.toInt |> Result.map (\hi -> Between lo hi))
                |> Result.withDefault term
                |> ElapsedTermPercentCondition

        SetMoreThan lo ->
            emptyToZero lo |> String.toInt |> Result.map MoreThan |> Result.withDefault term |> ElapsedTermPercentCondition

        ElapsedTermPercentNoOp ->
            ElapsedTermPercentCondition term


form : ElapsedTermPercentCondition -> Html ElapsedTermPercentMsg
form (ElapsedTermPercentCondition elapsedTermPercent) =
    let
        ( ltVal, btwMinVal, btwMaxVal, mtVal ) =
            case elapsedTermPercent of
                LessThan x ->
                    ( zeroToEmpty x, "", "", "" )

                Between mi ma ->
                    ( "", zeroToEmpty mi, zeroToEmpty ma, "" )

                MoreThan x ->
                    ( "", "", "", zeroToEmpty x )

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled elapsedTermPercent
    in
    Html.div []
        [ Form.formInline [ onSubmit ElapsedTermPercentNoOp ]
            [ elapsedTermPercentRadio ltEnabled (SetLessThan "0") "méně než" "etp1"
            , numericInput SetLessThan ltEnabled ltVal
            , text "% splátek"
            ]
        , Form.formInline [ onSubmit ElapsedTermPercentNoOp ]
            [ elapsedTermPercentRadio btwEnabled (SetBetween "0" "0") "je" "etp2"
            , numericInput (\x -> SetBetween x btwMaxVal) btwEnabled btwMinVal
            , text "až"
            , numericInput (\y -> SetBetween btwMinVal y) btwEnabled btwMaxVal
            , text "% splátek"
            ]
        , Form.formInline [ onSubmit ElapsedTermPercentNoOp ]
            [ elapsedTermPercentRadio mtEnabled (SetMoreThan "0") "více než" "etp3"
            , numericInput SetMoreThan mtEnabled mtVal
            , text "% splátek"
            ]
        ]


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
            Encode.list [ Encode.int 1, Encode.int x ]

        Between x y ->
            Encode.list [ Encode.int 2, Encode.int x, Encode.int y ]

        MoreThan y ->
            Encode.list [ Encode.int 3, Encode.int y ]


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
                        Decode.fail <| "Unable to decode ElapsedTermPercent from " ++ toString ints
            )


conditionDecoder : Decoder ElapsedTermPercentCondition
conditionDecoder =
    Decode.map ElapsedTermPercentCondition elapsedTermPercentDecoder
