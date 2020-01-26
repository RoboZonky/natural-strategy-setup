module Data.Filter.Conditions.SaleFee exposing
    ( SaleFee(..)
    , SaleFeeCondition(..)
    , conditionDecoder
    , defaultCondition
    , encodeCondition
    , encodeInsurance
    , form
    , insuranceDecoder
    , renderCondition
    )

import Bootstrap.Form.Checkbox as Checkbox
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type SaleFee
    = NoFee
    | WithFee


type SaleFeeCondition
    = SaleFeeCondition SaleFee


defaultCondition : SaleFeeCondition
defaultCondition =
    SaleFeeCondition NoFee


fromBool : Bool -> SaleFee
fromBool withFee =
    if withFee then
        WithFee

    else
        NoFee


renderCondition : SaleFeeCondition -> String
renderCondition (SaleFeeCondition saleFee) =
    let
        verb =
            case saleFee of
                NoFee ->
                    "není"

                WithFee ->
                    "je"
    in
    String.join " " [ "prodej", verb, "zpoplatněn" ]


form : SaleFeeCondition -> Html SaleFeeCondition
form (SaleFeeCondition sf) =
    Checkbox.checkbox
        [ Checkbox.id "sale_fee"
        , Checkbox.checked (sf == WithFee)
        , Checkbox.inline
        , Checkbox.onCheck (SaleFeeCondition << fromBool)
        ]
        "prodej je zpoplatněn"


encodeInsurance : SaleFee -> Value
encodeInsurance saleFee =
    case saleFee of
        WithFee ->
            Encode.bool True

        NoFee ->
            Encode.bool False


encodeCondition : SaleFeeCondition -> Value
encodeCondition (SaleFeeCondition c) =
    encodeInsurance c


insuranceDecoder : Decoder SaleFee
insuranceDecoder =
    Decode.map fromBool Decode.bool


conditionDecoder : Decoder SaleFeeCondition
conditionDecoder =
    Decode.map SaleFeeCondition insuranceDecoder
