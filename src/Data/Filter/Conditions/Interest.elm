module Data.Filter.Conditions.Interest
    exposing
        ( Interest(..)
        , InterestCondition(..)
        , InterestMsg
        , interestForm
        , interestToString
        , renderInterestCondition
        , update
        , validationErrors
        )

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Html exposing (Html, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onSubmit)
import Util exposing (emptyToZero, zeroToEmpty)


type Interest
    = LessThan Float
    | Between Float Float
    | MoreThan Float


type InterestCondition
    = InterestCondition Interest


defaultInterestCondition : InterestCondition
defaultInterestCondition =
    InterestCondition (MoreThan 0)


interestToString : Interest -> String
interestToString interest =
    case interest of
        LessThan maxBound ->
            "nedosahuje " ++ floatToString maxBound

        Between minBound maxBound ->
            "je " ++ floatToString minBound ++ " až " ++ floatToString maxBound

        MoreThan minBound ->
            "přesahuje " ++ floatToString minBound


renderInterestCondition : InterestCondition -> String
renderInterestCondition (InterestCondition interest) =
    "úrok " ++ interestToString interest ++ " % p.a"


floatToString : Float -> String
floatToString =
    -- toString for float has '.'. Replace it with ','
    String.map
        (\c ->
            if c == '.' then
                ','
            else
                c
        )
        << toString


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
update msg ((InterestCondition i) as ic) =
    case msg of
        SetLessThan hi ->
            emptyToZero hi |> String.toFloat |> Result.map (InterestCondition << LessThan) |> Result.withDefault ic

        SetBetween loStr hiStr ->
            emptyToZero loStr
                |> String.toFloat
                |> Result.andThen (\lo -> emptyToZero hiStr |> String.toFloat |> Result.map (\hi -> InterestCondition <| Between lo hi))
                |> Result.withDefault ic

        SetMoreThan lo ->
            emptyToZero lo |> String.toFloat |> Result.map (InterestCondition << MoreThan) |> Result.withDefault ic

        InterestNoOp ->
            ic


interestForm : InterestCondition -> Html InterestMsg
interestForm (InterestCondition interest) =
    let
        ( ltVal, btwMinVal, btwMaxVal, mtVal ) =
            case interest of
                LessThan x ->
                    ( zeroToEmpty x, "", "", "" )

                Between mi ma ->
                    ( "", zeroToEmpty mi, zeroToEmpty ma, "" )

                MoreThan x ->
                    ( "", "", "", zeroToEmpty x )

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled interest
    in
    Form.form [ onSubmit InterestNoOp ]
        [ Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio ltEnabled (SetLessThan "0") "nedosahuje"
            , numericInput SetLessThan ltEnabled ltVal
            , text "%"
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio btwEnabled (SetBetween "0" "0") "je"
            , numericInput (\x -> SetBetween x btwMaxVal) btwEnabled btwMinVal
            , text "až"
            , numericInput (\y -> SetBetween btwMinVal y) btwEnabled btwMaxVal
            , text "%"
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio mtEnabled (SetMoreThan "0") "přesahuje"
            , numericInput SetMoreThan mtEnabled mtVal
            , text "%"
            ]
        ]


numericInput : (String -> InterestMsg) -> Bool -> String -> Html InterestMsg
numericInput msg enabled value =
    Input.number
        [ Input.small
        , Input.onInput msg
        , Input.disabled <| not enabled
        , Input.value value
        , Input.attrs [ Attr.min "0", Attr.max "100", class "mx-1" ]
        ]


interestRadio : Bool -> InterestMsg -> String -> Html InterestMsg
interestRadio checked msg label =
    Radio.radio
        [ Radio.name "interest"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label
