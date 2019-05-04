module Data.Filter.Conditions.SaleFee exposing
    ( SaleFee(..)
    , SaleFeeCondition(..)
    , SaleFeeMsg(..)
    , conditionDecoder
    , defaultCondition
    , encodeCondition
    , encodeInsurance
    , form
    , insuranceDecoder
    , renderCondition
    , update
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


type SaleFeeMsg
    = SetSaleFee SaleFee


update : SaleFeeMsg -> SaleFeeCondition -> SaleFeeCondition
update (SetSaleFee sf) _ =
    SaleFeeCondition sf


form : SaleFeeCondition -> Html SaleFeeMsg
form (SaleFeeCondition sf) =
    Checkbox.checkbox
        [ Checkbox.inline
        , Checkbox.checked (sf == WithFee)
        , Checkbox.onCheck
            (\checked ->
                if checked then
                    SetSaleFee WithFee

                else
                    SetSaleFee NoFee
            )
        ]
        "prodej je zpoplatněn"



-- JSON


encodeInsurance : SaleFee -> Value
encodeInsurance saleFee =
    case saleFee of
        NoFee ->
            Encode.bool False

        WithFee ->
            Encode.bool True


encodeCondition : SaleFeeCondition -> Value
encodeCondition (SaleFeeCondition c) =
    encodeInsurance c


insuranceDecoder : Decoder SaleFee
insuranceDecoder =
    Decode.bool
        |> Decode.map
            (\b ->
                if b then
                    WithFee

                else
                    NoFee
            )


conditionDecoder : Decoder SaleFeeCondition
conditionDecoder =
    Decode.map SaleFeeCondition insuranceDecoder
