module Data.Filter.Conditions.RevenueRate exposing
    ( RevenueRate(..)
    , RevenueRateCondition(..)
    , RevenueRateMsg
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
import Html exposing (Html, text)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (formatPercentage, parseFloat, zeroToEmptyFloat)
import View.NumericInput


type RevenueRate
    = LessThan Float
    | Between Float Float
    | MoreThan Float


type RevenueRateCondition
    = RevenueRateCondition RevenueRate


defaultCondition : RevenueRateCondition
defaultCondition =
    RevenueRateCondition (MoreThan 0)


renderCondition : RevenueRateCondition -> String
renderCondition (RevenueRateCondition revenueRate) =
    "optimální výnos  " ++ revenueRateToString revenueRate ++ " % p.a."


revenueRateToString : RevenueRate -> String
revenueRateToString revenueRate =
    case revenueRate of
        Between from to ->
            "je " ++ formatPercentage from ++ " až " ++ formatPercentage to

        MoreThan lowerBound ->
            "přesahuje " ++ formatPercentage lowerBound

        LessThan upperBound ->
            "nedosahuje " ++ formatPercentage upperBound


validationErrors : RevenueRateCondition -> List String
validationErrors (RevenueRateCondition a) =
    case a of
        LessThan x ->
            positive x

        Between x y ->
            positive x ++ positive y ++ minNotGtMax x y

        MoreThan x ->
            positive x


positive : Float -> List String
positive =
    Validate.positive "Optimální výnos"


minNotGtMax : Float -> Float -> List String
minNotGtMax =
    Validate.minNotGtMax "Optimální výnos"


type RevenueRateMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | RevenueRateNoOp


msgToModel : RevenueRateMsg -> Maybe RevenueRate
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseFloat hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseFloat lo) (parseFloat hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseFloat lo)

        RevenueRateNoOp ->
            Nothing


update : RevenueRateMsg -> RevenueRateCondition -> RevenueRateCondition
update msg (RevenueRateCondition rrc) =
    msgToModel msg
        |> Maybe.withDefault rrc
        |> RevenueRateCondition


type alias RevenueRateRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : RevenueRateCondition -> Html RevenueRateMsg
form (RevenueRateCondition rrc) =
    let
        values =
            case rrc of
                LessThan x ->
                    RevenueRateRadioValues (zeroToEmptyFloat x) "" "" ""

                Between mi ma ->
                    RevenueRateRadioValues "" (zeroToEmptyFloat mi) (zeroToEmptyFloat ma) ""

                MoreThan x ->
                    RevenueRateRadioValues "" "" "" (zeroToEmptyFloat x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled rrc
    in
    Html.div []
        [ Form.formInline [ onSubmit RevenueRateNoOp ]
            [ revenueRateRadio ltEnabled (SetLessThan "0") "nedosahuje" "revenueRate1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit RevenueRateNoOp ]
            [ revenueRateRadio btwEnabled (SetBetween "0" "0") "je" "revenueRate2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit RevenueRateNoOp ]
            [ revenueRateRadio mtEnabled (SetMoreThan "0") "přesahuje" "revenueRate3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


numericInput : (String -> RevenueRateMsg) -> Bool -> String -> Html RevenueRateMsg
numericInput =
    View.NumericInput.numericInput 0 1000000


unit : Html msg
unit =
    text " % p.a."


revenueRateRadio : Bool -> RevenueRateMsg -> String -> DomId -> Html RevenueRateMsg
revenueRateRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "revenueRate"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label


whichEnabled : RevenueRate -> ( Bool, Bool, Bool )
whichEnabled rr =
    case rr of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )



-- JSON


encodeCondition : RevenueRateCondition -> Value
encodeCondition (RevenueRateCondition c) =
    encodeRevenueRate c


encodeRevenueRate : RevenueRate -> Value
encodeRevenueRate i =
    case i of
        LessThan x ->
            Encode.object [ ( "a", Encode.int 1 ), ( "b", Encode.float x ) ]

        Between x y ->
            Encode.object [ ( "a", Encode.int 2 ), ( "b", Encode.float x ), ( "c", Encode.float y ) ]

        MoreThan y ->
            Encode.object [ ( "a", Encode.int 3 ), ( "b", Encode.float y ) ]


revenueRateDecoder : Decoder RevenueRate
revenueRateDecoder =
    Decode.field "a" Decode.int
        |> Decode.andThen
            (\typ ->
                case typ of
                    1 ->
                        Decode.map LessThan
                            (Decode.field "b" Decode.float)

                    2 ->
                        Decode.map2 Between
                            (Decode.field "b" Decode.float)
                            (Decode.field "c" Decode.float)

                    3 ->
                        Decode.map MoreThan
                            (Decode.field "b" Decode.float)

                    _ ->
                        Decode.fail <| "Invalid RevenueRate type " ++ String.fromInt typ
            )


conditionDecoder : Decoder RevenueRateCondition
conditionDecoder =
    Decode.map RevenueRateCondition revenueRateDecoder
