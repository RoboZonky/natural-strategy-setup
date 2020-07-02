module Data.Filter.Conditions.RelativeSaleDiscount exposing
    ( RelativeSaleDiscount(..)
    , RelativeSaleDiscountCondition(..)
    , RelativeSaleDiscountMsg
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
import Html exposing (Html)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (parseInt, zeroToEmpty)
import View.NumericInput


type RelativeSaleDiscount
    = LessThan Int
    | Between Int Int
    | MoreThan Int
    | WithDiscount
    | WithoutDiscount


type RelativeSaleDiscountCondition
    = RelativeSaleDiscountCondition RelativeSaleDiscount


defaultCondition : RelativeSaleDiscountCondition
defaultCondition =
    RelativeSaleDiscountCondition (LessThan 0)


relativeSaleDiscountToString : RelativeSaleDiscount -> String
relativeSaleDiscountToString relativeSaleDiscount =
    let
        prefix =
            "sleva "

        suffix =
            " % zbývající jistiny"
    in
    case relativeSaleDiscount of
        LessThan maxBound ->
            prefix ++ "nedosahuje " ++ String.fromInt maxBound ++ suffix

        Between minBound maxBound ->
            prefix ++ "je " ++ String.fromInt minBound ++ " až " ++ String.fromInt maxBound ++ suffix

        MoreThan minBound ->
            prefix ++ "přesahuje " ++ String.fromInt minBound ++ suffix

        WithDiscount ->
            "se slevou"

        WithoutDiscount ->
            "bez slevy"


renderCondition : RelativeSaleDiscountCondition -> String
renderCondition (RelativeSaleDiscountCondition term) =
    relativeSaleDiscountToString term


validationErrors : RelativeSaleDiscountCondition -> List String
validationErrors (RelativeSaleDiscountCondition t) =
    case t of
        LessThan x ->
            validateInRange 1 100 x

        Between x y ->
            validateInRange 0 99 x ++ validateInRange 1 100 y ++ minNotGtMax x y

        MoreThan x ->
            validateInRange 0 99 x

        WithDiscount ->
            []

        WithoutDiscount ->
            []


validateInRange : Int -> Int -> Int -> List String
validateInRange =
    Validate.intInRange "Sleva"


minNotGtMax : Int -> Int -> List String
minNotGtMax =
    Validate.minNotGtMax "Sleva"


type RelativeSaleDiscountMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | SetWithDiscount
    | SetWithoutDiscount
    | RelativeSaleDiscountNoOp


type alias WhichEnabled =
    { ltEnabled : Bool
    , btwEnabled : Bool
    , mtEnabled : Bool
    , withEnabled : Bool
    , withoutEnabled : Bool
    }


whichEnabled : RelativeSaleDiscount -> WhichEnabled
whichEnabled relativeSaleDiscount =
    case relativeSaleDiscount of
        LessThan _ ->
            WhichEnabled True False False False False

        Between _ _ ->
            WhichEnabled False True False False False

        MoreThan _ ->
            WhichEnabled False False True False False

        WithDiscount ->
            WhichEnabled False False False True False

        WithoutDiscount ->
            WhichEnabled False False False False True


msgToModel : RelativeSaleDiscountMsg -> Maybe RelativeSaleDiscount
msgToModel msg =
    case msg of
        SetLessThan hi ->
            Maybe.map LessThan (parseInt hi)

        SetBetween lo hi ->
            Maybe.map2 Between (parseInt lo) (parseInt hi)

        SetMoreThan lo ->
            Maybe.map MoreThan (parseInt lo)

        SetWithDiscount ->
            Just WithDiscount

        SetWithoutDiscount ->
            Just WithoutDiscount

        RelativeSaleDiscountNoOp ->
            Nothing


update : RelativeSaleDiscountMsg -> RelativeSaleDiscountCondition -> RelativeSaleDiscountCondition
update msg (RelativeSaleDiscountCondition term) =
    msgToModel msg
        |> Maybe.withDefault term
        |> RelativeSaleDiscountCondition



-- Elm 0.19: using this instead of 4-tuple


type alias RelativeSaleDiscountRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : RelativeSaleDiscountCondition -> Html RelativeSaleDiscountMsg
form (RelativeSaleDiscountCondition relativeSaleDiscount) =
    let
        values =
            case relativeSaleDiscount of
                LessThan x ->
                    RelativeSaleDiscountRadioValues (zeroToEmpty x) "" "" ""

                Between mi ma ->
                    RelativeSaleDiscountRadioValues "" (zeroToEmpty mi) (zeroToEmpty ma) ""

                MoreThan x ->
                    RelativeSaleDiscountRadioValues "" "" "" (zeroToEmpty x)

                WithDiscount ->
                    RelativeSaleDiscountRadioValues "" "" "" ""

                WithoutDiscount ->
                    RelativeSaleDiscountRadioValues "" "" "" ""

        { ltEnabled, btwEnabled, mtEnabled, withEnabled, withoutEnabled } =
            whichEnabled relativeSaleDiscount
    in
    Html.div []
        [ Form.formInline [ onSubmit RelativeSaleDiscountNoOp ]
            [ relativeSaleDiscountRadio ltEnabled (SetLessThan "0") "nedosahuje" "rsd1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit RelativeSaleDiscountNoOp ]
            [ relativeSaleDiscountRadio btwEnabled (SetBetween "0" "0") "je" "rsd2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , Html.text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit RelativeSaleDiscountNoOp ]
            [ relativeSaleDiscountRadio mtEnabled (SetMoreThan "0") "přesahuje" "rsd3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , unit
            ]
        , Form.formInline [ onSubmit RelativeSaleDiscountNoOp ]
            [ relativeSaleDiscountRadio withEnabled SetWithDiscount "se slevou" "rsd4" ]
        , Form.formInline [ onSubmit RelativeSaleDiscountNoOp ]
            [ relativeSaleDiscountRadio withoutEnabled SetWithoutDiscount "bez slevy" "rsd5" ]
        ]


unit : Html msg
unit =
    Html.text "% zbývající jistiny"


numericInput : (String -> RelativeSaleDiscountMsg) -> Bool -> String -> Html RelativeSaleDiscountMsg
numericInput =
    View.NumericInput.numericInput 0 100


relativeSaleDiscountRadio : Bool -> RelativeSaleDiscountMsg -> String -> DomId -> Html RelativeSaleDiscountMsg
relativeSaleDiscountRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "relativeSaleDiscount"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeRelativeSaleDiscount : RelativeSaleDiscount -> Value
encodeRelativeSaleDiscount amt =
    Encode.list Encode.int <|
        case amt of
            LessThan x ->
                [ 1, x ]

            Between x y ->
                [ 2, x, y ]

            MoreThan y ->
                [ 3, y ]

            WithDiscount ->
                [ 4 ]

            WithoutDiscount ->
                [ 5 ]


encodeCondition : RelativeSaleDiscountCondition -> Value
encodeCondition (RelativeSaleDiscountCondition c) =
    encodeRelativeSaleDiscount c


relativeSaleDiscountDecoder : Decoder RelativeSaleDiscount
relativeSaleDiscountDecoder =
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

                    [ 4 ] ->
                        Decode.succeed WithDiscount

                    [ 5 ] ->
                        Decode.succeed WithoutDiscount

                    _ ->
                        Decode.fail <| "Unable to decode RelativeSaleDiscount from " ++ Util.intListToString ints
            )


conditionDecoder : Decoder RelativeSaleDiscountCondition
conditionDecoder =
    Decode.map RelativeSaleDiscountCondition relativeSaleDiscountDecoder
