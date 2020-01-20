module Data.Filter.Conditions.RelativeProfit exposing
    ( RelativeProfit(..)
    , RelativeProfitCondition(..)
    , RelativeProfitMsg
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
import Util exposing (parseInt, zeroToEmpty)
import View.NumericInput


type RelativeProfit
    = LessThan Int
    | Between Int Int
    | MoreThan Int


type RelativeProfitCondition
    = RelativeProfitCondition RelativeProfit


defaultCondition : RelativeProfitCondition
defaultCondition =
    RelativeProfitCondition (LessThan 0)


relativeProfitToString : RelativeProfit -> String
relativeProfitToString relativeProfit =
    case relativeProfit of
        LessThan maxBound ->
            "nedosahuje " ++ String.fromInt maxBound

        Between minBound maxBound ->
            "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound

        MoreThan minBound ->
            "přesahuje " ++ String.fromInt minBound


renderCondition : RelativeProfitCondition -> String
renderCondition (RelativeProfitCondition term) =
    "dosažený výnos " ++ relativeProfitToString term ++ " % původní jistiny"


validationErrors : RelativeProfitCondition -> List String
validationErrors (RelativeProfitCondition t) =
    case t of
        LessThan x ->
            validateInRange 1 100 x

        Between x y ->
            validateInRange 0 99 x ++ validateInRange 1 100 y ++ minNotGtMax x y

        MoreThan x ->
            validateInRange 0 99 x


validateInRange : Int -> Int -> Int -> List String
validateInRange =
    Validate.intInRange "Dosažený výnos v procentech"


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Dosažený výnos v procentech"


type RelativeProfitMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | RelativeProfitNoOp


whichEnabled : RelativeProfit -> ( Bool, Bool, Bool )
whichEnabled relativeProfit =
    case relativeProfit of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


msgToModel : RelativeProfitMsg -> Maybe RelativeProfit
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        RelativeProfitNoOp ->
            Nothing


update : RelativeProfitMsg -> RelativeProfitCondition -> RelativeProfitCondition
update msg (RelativeProfitCondition term) =
    msgToModel msg
        |> Maybe.withDefault term
        |> RelativeProfitCondition


type alias RelativeProfitRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : RelativeProfitCondition -> Html RelativeProfitMsg
form (RelativeProfitCondition relativeProfit) =
    let
        values =
            case relativeProfit of
                LessThan x ->
                    RelativeProfitRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    RelativeProfitRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    RelativeProfitRadioValues "" "" "" (zeroToEmpty x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled relativeProfit
    in
    Html.div []
        [ Form.formInline [ onSubmit RelativeProfitNoOp ]
            [ relativeProfitRadio ltEnabled (SetLessThan "0") "nedosahuje" "rp1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit RelativeProfitNoOp ]
            [ relativeProfitRadio btwEnabled (SetBetween "0" "0") "je" "rp2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit RelativeProfitNoOp ]
            [ relativeProfitRadio mtEnabled (SetMoreThan "0") "více než" "rp3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    text "% původní jistiny"


numericInput : (String -> RelativeProfitMsg) -> Bool -> String -> Html RelativeProfitMsg
numericInput =
    View.NumericInput.numericInput 0 100


relativeProfitRadio : Bool -> RelativeProfitMsg -> String -> DomId -> Html RelativeProfitMsg
relativeProfitRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "relativeProfit"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeRelativeProfit : RelativeProfit -> Value
encodeRelativeProfit amt =
    case amt of
        LessThan x ->
            Encode.list Encode.int [ 1, x ]

        Between x y ->
            Encode.list Encode.int [ 2, x, y ]

        MoreThan y ->
            Encode.list Encode.int [ 3, y ]


encodeCondition : RelativeProfitCondition -> Value
encodeCondition (RelativeProfitCondition c) =
    encodeRelativeProfit c


relativeProfitDecoder : Decoder RelativeProfit
relativeProfitDecoder =
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
                        Decode.fail <| "Unable to decode RelativeProfit from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder RelativeProfitCondition
conditionDecoder =
    Decode.map RelativeProfitCondition relativeProfitDecoder
