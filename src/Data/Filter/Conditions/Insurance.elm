module Data.Filter.Conditions.Insurance exposing
    ( Insurance(..)
    , InsuranceCondition(..)
    , conditionDecoder
    , defaultCondition
    , encodeCondition
    , form
    , renderCondition
    )

import Bootstrap.Form.Checkbox as Checkbox
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Insurance
    = Active
    | Inactive


type InsuranceCondition
    = InsuranceCondition Insurance


defaultCondition : InsuranceCondition
defaultCondition =
    InsuranceCondition Active


fromBool : Bool -> Insurance
fromBool isActive =
    if isActive then
        Active

    else
        Inactive


renderCondition : InsuranceCondition -> String
renderCondition (InsuranceCondition insurance) =
    let
        verb =
            case insurance of
                Active ->
                    "je"

                Inactive ->
                    "není"
    in
    String.join " " [ "pojištění", verb, "aktivní" ]


form : InsuranceCondition -> Html InsuranceCondition
form (InsuranceCondition i) =
    Checkbox.checkbox
        [ Checkbox.id "insurance"
        , Checkbox.checked (i == Active)
        , Checkbox.inline
        , Checkbox.onCheck (InsuranceCondition << fromBool)
        ]
        "pojištění je aktivní"


encodeInsurance : Insurance -> Value
encodeInsurance ins =
    case ins of
        Active ->
            Encode.bool True

        Inactive ->
            Encode.bool False


encodeCondition : InsuranceCondition -> Value
encodeCondition (InsuranceCondition c) =
    encodeInsurance c


insuranceDecoder : Decoder Insurance
insuranceDecoder =
    Decode.map fromBool Decode.bool


conditionDecoder : Decoder InsuranceCondition
conditionDecoder =
    Decode.map InsuranceCondition insuranceDecoder
