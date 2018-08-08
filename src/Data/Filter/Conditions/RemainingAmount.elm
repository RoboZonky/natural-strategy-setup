module Data.Filter.Conditions.RemainingAmount
    exposing
        ( RemainingAmount(Between, LessThan, MoreThan)
        , RemainingAmountCondition(RemainingAmountCondition)
        , RemainingAmountMsg
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


type RemainingAmount
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type RemainingAmountCondition
    = RemainingAmountCondition RemainingAmount


defaultCondition : RemainingAmountCondition
defaultCondition =
    RemainingAmountCondition (LessThan 0)


renderCondition : RemainingAmountCondition -> String
renderCondition (RemainingAmountCondition remainingAmount) =
    "zbývající jistina " ++ remainingAmountToString remainingAmount ++ " Kč"


remainingAmountToString : RemainingAmount -> String
remainingAmountToString remainingAmount =
    case remainingAmount of
        Between from to ->
            "je " ++ toString from ++ " až " ++ toString to

        MoreThan lowerBound ->
            "přesahuje " ++ toString lowerBound

        LessThan upperBound ->
            "nedosahuje " ++ toString upperBound


validationErrors : RemainingAmountCondition -> List String
validationErrors (RemainingAmountCondition a) =
    case a of
        LessThan x ->
            validateInt x

        Between x y ->
            validateInt x ++ validateInt y ++ validateMinNotGtMax x y

        MoreThan x ->
            validateInt x


validateInt : Int -> List String
validateInt x =
    Util.validate (x < 0) "Zbývající jistina: musí být kladné číslo"


validateMinNotGtMax : Int -> Int -> List String
validateMinNotGtMax minBound maxBound =
    Util.validate (minBound > maxBound) "Zbývající jistina: minimum nesmí být větší než maximum"


type RemainingAmountMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | RemainingAmountNoOp


whichEnabled : RemainingAmount -> ( Bool, Bool, Bool )
whichEnabled amt =
    case amt of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


update : RemainingAmountMsg -> RemainingAmountCondition -> RemainingAmountCondition
update msg ac =
    case msg of
        SetLessThan hi ->
            emptyToZero hi |> String.toInt |> Result.map (RemainingAmountCondition << LessThan) |> Result.withDefault ac

        SetBetween loStr hiStr ->
            emptyToZero loStr
                |> String.toInt
                |> Result.andThen (\lo -> emptyToZero hiStr |> String.toInt |> Result.map (\hi -> RemainingAmountCondition <| Between lo hi))
                |> Result.withDefault ac

        SetMoreThan lo ->
            emptyToZero lo |> String.toInt |> Result.map (RemainingAmountCondition << MoreThan) |> Result.withDefault ac

        RemainingAmountNoOp ->
            ac


form : RemainingAmountCondition -> Html RemainingAmountMsg
form (RemainingAmountCondition ramt) =
    let
        ( ltVal, btwMinVal, btwMaxVal, mtVal ) =
            case ramt of
                LessThan x ->
                    ( zeroToEmpty x, "", "", "" )

                Between mi ma ->
                    ( "", zeroToEmpty mi, zeroToEmpty ma, "" )

                MoreThan x ->
                    ( "", "", "", zeroToEmpty x )

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled ramt
    in
    Html.div []
        [ Form.formInline [ onSubmit RemainingAmountNoOp ]
            [ remainingAmountRadio ltEnabled (SetLessThan "0") "nedosahuje" "ramount1"
            , numericInput SetLessThan ltEnabled ltVal
            , text "Kč"
            ]
        , Form.formInline [ onSubmit RemainingAmountNoOp ]
            [ remainingAmountRadio btwEnabled (SetBetween "0" "0") "je" "ramount2"
            , numericInput (\x -> SetBetween x btwMaxVal) btwEnabled btwMinVal
            , text "až"
            , numericInput (\y -> SetBetween btwMinVal y) btwEnabled btwMaxVal
            , text "Kč"
            ]
        , Form.formInline [ onSubmit RemainingAmountNoOp ]
            [ remainingAmountRadio mtEnabled (SetMoreThan "0") "přesahuje" "ramount3"
            , numericInput SetMoreThan mtEnabled mtVal
            , text "Kč"
            ]
        ]


numericInput : (String -> RemainingAmountMsg) -> Bool -> String -> Html RemainingAmountMsg
numericInput =
    View.NumericInput.numericInput 0 1000000


remainingAmountRadio : Bool -> RemainingAmountMsg -> String -> DomId -> Html RemainingAmountMsg
remainingAmountRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "ramount"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeAmount : RemainingAmount -> Value
encodeAmount amt =
    case amt of
        LessThan x ->
            Encode.list [ Encode.int 1, Encode.int x ]

        Between x y ->
            Encode.list [ Encode.int 2, Encode.int x, Encode.int y ]

        MoreThan y ->
            Encode.list [ Encode.int 3, Encode.int y ]


encodeCondition : RemainingAmountCondition -> Value
encodeCondition (RemainingAmountCondition c) =
    encodeAmount c


remainingAmountDecoder : Decoder RemainingAmount
remainingAmountDecoder =
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
                        Decode.fail <| "Unable to decode RemainingAmount from " ++ toString ints
            )


conditionDecoder : Decoder RemainingAmountCondition
conditionDecoder =
    Decode.map RemainingAmountCondition remainingAmountDecoder
