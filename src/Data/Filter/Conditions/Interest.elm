module Data.Filter.Conditions.Interest exposing
    ( Interest(..)
    , InterestCondition(..)
    , InterestMsg
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
import Util exposing (emptyToZero, zeroToEmptyFloat)
import View.NumericInput


type Interest
    = LessThan Float
    | Between Float Float
    | MoreThan Float


type InterestCondition
    = InterestCondition Interest


defaultCondition : InterestCondition
defaultCondition =
    InterestCondition (LessThan 0)


toString : Interest -> String
toString interest =
    case interest of
        LessThan maxBound ->
            "nedosahuje " ++ floatToString maxBound

        Between minBound maxBound ->
            "je " ++ floatToString minBound ++ " až " ++ floatToString maxBound

        MoreThan minBound ->
            "přesahuje " ++ floatToString minBound


renderCondition : InterestCondition -> String
renderCondition (InterestCondition interest) =
    "úrok " ++ toString interest ++ " % p.a"


{-| RoboZonky requires the interest to ALWAYS contain decimal comma,
so add it there in cases when the interest is "integral"
-}
floatToString : Float -> String
floatToString float =
    let
        strFloat =
            String.fromFloat float
    in
    case String.split "." strFloat of
        [ beforeComma, afterComma ] ->
            beforeComma ++ "," ++ afterComma

        _ ->
            strFloat ++ ",0"


validationErrors : InterestCondition -> List String
validationErrors (InterestCondition ic) =
    case ic of
        LessThan maxBound ->
            validatePercent maxBound

        Between minBound maxBound ->
            validatePercent minBound
                ++ validatePercent maxBound
                ++ validateMinNotGtMax minBound maxBound

        MoreThan minBound ->
            validatePercent minBound


validatePercent : Float -> List String
validatePercent x =
    Util.validate (x < 0 || 100 < x) "Úrok: musí být v rozmezí 0 až 100%"


validateMinNotGtMax : Float -> Float -> List String
validateMinNotGtMax minBound maxBound =
    Util.validate (minBound > maxBound) "Úrok: minimum nesmí být větší než maximum"


type InterestMsg
    = SetLessThan String
    | SetBetween String String
    | SetMoreThan String
    | InterestNoOp


whichEnabled : Interest -> ( Bool, Bool, Bool )
whichEnabled interest =
    case interest of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


update : InterestMsg -> InterestCondition -> InterestCondition
update msg (InterestCondition i) =
    InterestCondition <|
        Maybe.withDefault i <|
            case msg of
                SetLessThan hi ->
                    Maybe.map LessThan (parseFloat hi)

                SetBetween lo hi ->
                    Maybe.map2 Between (parseFloat lo) (parseFloat hi)

                SetMoreThan lo ->
                    Maybe.map MoreThan (parseFloat lo)

                InterestNoOp ->
                    Nothing


parseFloat : String -> Maybe Float
parseFloat =
    String.toFloat << emptyToZero


type alias InterestRadioValues =
    { lessThan : String
    , betweenMin : String
    , betweenMax : String
    , moreThan : String
    }


form : InterestCondition -> Html InterestMsg
form (InterestCondition interest) =
    let
        values =
            case interest of
                LessThan x ->
                    InterestRadioValues (zeroToEmptyFloat x) "" "" ""

                Between mi ma ->
                    InterestRadioValues "" (zeroToEmptyFloat mi) (zeroToEmptyFloat ma) ""

                MoreThan x ->
                    InterestRadioValues "" "" "" (zeroToEmptyFloat x)

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled interest
    in
    Html.div []
        [ Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio ltEnabled (SetLessThan "0") "nedosahuje" "interest1"
            , numericInput SetLessThan ltEnabled values.lessThan
            , text "%"
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio btwEnabled (SetBetween "0" "0") "je" "interest2"
            , numericInput (\x -> SetBetween x values.betweenMax) btwEnabled values.betweenMin
            , text "až"
            , numericInput (\y -> SetBetween values.betweenMin y) btwEnabled values.betweenMax
            , text "%"
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio mtEnabled (SetMoreThan "0") "přesahuje" "interest3"
            , numericInput SetMoreThan mtEnabled values.moreThan
            , text "%"
            ]
        ]


numericInput : (String -> InterestMsg) -> Bool -> String -> Html InterestMsg
numericInput =
    View.NumericInput.numericInput 0 100


interestRadio : Bool -> InterestMsg -> String -> DomId -> Html InterestMsg
interestRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "interest"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeInterest : Interest -> Value
encodeInterest i =
    case i of
        LessThan x ->
            Encode.object [ ( "v", Encode.int 1 ), ( "w", Encode.float x ) ]

        Between x y ->
            Encode.object [ ( "v", Encode.int 2 ), ( "x", Encode.float x ), ( "y", Encode.float y ) ]

        MoreThan y ->
            Encode.object [ ( "v", Encode.int 3 ), ( "w", Encode.float y ) ]


encodeCondition : InterestCondition -> Value
encodeCondition (InterestCondition c) =
    encodeInterest c


interestDecoder : Decoder Interest
interestDecoder =
    Decode.field "v" Decode.int
        |> Decode.andThen
            (\typ ->
                case typ of
                    1 ->
                        Decode.map LessThan
                            (Decode.field "w" Decode.float)

                    2 ->
                        Decode.map2 Between
                            (Decode.field "x" Decode.float)
                            (Decode.field "y" Decode.float)

                    3 ->
                        Decode.map MoreThan
                            (Decode.field "w" Decode.float)

                    _ ->
                        Decode.fail <| "Invalid interest type " ++ String.fromInt typ
            )


conditionDecoder : Decoder InterestCondition
conditionDecoder =
    Decode.map InterestCondition interestDecoder
