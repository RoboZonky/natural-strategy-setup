module Data.Filter.Conditions.Insurance exposing
    ( Insurance(..)
    , InsuranceCondition(..)
    , InsuranceMsg
    , conditionDecoder
    , defaultCondition
    , encodeCondition
    , form
    , renderCondition
    , update
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


type InsuranceMsg
    = SetInsurance Insurance


update : InsuranceMsg -> InsuranceCondition -> InsuranceCondition
update (SetInsurance i) _ =
    InsuranceCondition i


form : InsuranceCondition -> Html InsuranceMsg
form (InsuranceCondition i) =
    Checkbox.checkbox
        [ Checkbox.id "insurance"
        , Checkbox.checked (i == Active)
        , Checkbox.inline
        , Checkbox.onCheck
            (\checked ->
                if checked then
                    SetInsurance Active

                else
                    SetInsurance Inactive
            )
        ]
        "pojištění je aktivní"



-- JSON


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
    Decode.bool
        |> Decode.map
            (\b ->
                if b then
                    Active

                else
                    Inactive
            )


conditionDecoder : Decoder InsuranceCondition
conditionDecoder =
    Decode.map InsuranceCondition insuranceDecoder
