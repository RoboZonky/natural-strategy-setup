module Data.Filter.Conditions.RemainingAmount exposing
    ( RemainingAmount(..)
    , RemainingAmountCondition(..)
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
import Util exposing (parseInt, zeroToEmpty)
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
            "je " ++ String.fromInt from ++ " až " ++ String.fromInt to

        MoreThan lowerBound ->
            "přesahuje " ++ String.fromInt lowerBound

        LessThan upperBound ->
            "nedosahuje " ++ String.fromInt upperBound


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
update msg (RemainingAmountCondition ra) =
    RemainingAmountCondition <|
        Maybe.withDefault ra <|
            case msg of
                SetLessThan hi ->
                    Maybe.map LessThan (parseInt hi)

                SetBetween lo hi ->
                    Maybe.map2 Between (parseInt lo) (parseInt hi)

                SetMoreThan lo ->
                    Maybe.map MoreThan (parseInt lo)

                RemainingAmountNoOp ->
                    Nothing


type alias RemainingAmountRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : RemainingAmountCondition -> Html RemainingAmountMsg
form (RemainingAmountCondition ramt) =
    let
        values =
            case ramt of
                LessThan x ->
                    RemainingAmountRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    RemainingAmountRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    RemainingAmountRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled ramt
    in
    Html.div []
        [ Form.formInline [ onSubmit RemainingAmountNoOp ]
            [ remainingAmountRadio ltEnabled (SetLessThan "0") "nedosahuje" "ramount1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit RemainingAmountNoOp ]
            [ remainingAmountRadio btwEnabled (SetBetween "0" "0") "je" "ramount2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit RemainingAmountNoOp ]
            [ remainingAmountRadio mtEnabled (SetMoreThan "0") "přesahuje" "ramount3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    text "Kč"


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
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


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
                        Decode.fail <| "Unable to decode RemainingAmount from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder RemainingAmountCondition
conditionDecoder =
    Decode.map RemainingAmountCondition remainingAmountDecoder
