module Data.Filter.Conditions.Health exposing
    ( Health(..)
    , HealthCondition(..)
    , HealthMsg
    , allHealths
    , conditionDecoder
    , defaultCondition
    , encodeCondition
    , form
    , renderCondition
    , update
    , validationErrors
    )

import Bootstrap.Form.Checkbox as Checkbox
import Data.Validate as Validate
import Html exposing (Html, div)
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type Health
    = Healthy
    | CurrentlyInDue
    | HistoricallyInDue


allHealths : List Health
allHealths =
    [ Healthy
    , CurrentlyInDue
    , HistoricallyInDue
    ]


type HealthCondition
    = HealthList (List Health)


defaultCondition : HealthCondition
defaultCondition =
    HealthList []


healthToString : Health -> String
healthToString income =
    case income of
        Healthy ->
            "nikdy nebyla"

        HistoricallyInDue ->
            "nyní není"

        CurrentlyInDue ->
            "nyní je"


renderCondition : HealthCondition -> String
renderCondition (HealthList list) =
    Util.orList healthToString list ++ " po splatnosti"


validationErrors : HealthCondition -> List String
validationErrors (HealthList rlist) =
    Validate.isNotEmpty "Historie splácení" rlist


type HealthMsg
    = AddHealth Health
    | RemoveHealth Health


update : HealthMsg -> HealthCondition -> HealthCondition
update msg (HealthList ilist) =
    case msg of
        AddHealth i ->
            HealthList (i :: ilist)

        RemoveHealth i ->
            HealthList (List.filter (\ii -> ii /= i) ilist)


form : HealthCondition -> Html HealthMsg
form (HealthList ilist) =
    div []
        (List.indexedMap
            (\index income -> healthCheckbox index income (List.member income ilist))
            allHealths
            ++ [ Html.span
                    [ Attr.style "position" "relative"
                    , Attr.style "bottom" "2px"
                    ]
                    [ Html.text "... po splatnosti" ]
               ]
        )


healthCheckbox : Int -> Health -> Bool -> Html HealthMsg
healthCheckbox index income isEnabled =
    Checkbox.checkbox
        [ Checkbox.id ("health_" ++ String.fromInt index)
        , Checkbox.checked isEnabled
        , Checkbox.inline
        , Checkbox.onCheck
            (\checked ->
                if checked then
                    AddHealth income

                else
                    RemoveHealth income
            )
        ]
        (healthToString income)



-- JSON


encodeHealth : Health -> Value
encodeHealth =
    Util.enumEncoder allHealths


encodeCondition : HealthCondition -> Value
encodeCondition (HealthList mis) =
    Encode.list encodeHealth mis


incomeDecoder : Decoder Health
incomeDecoder =
    Util.enumDecoder "Health" allHealths


conditionDecoder : Decoder HealthCondition
conditionDecoder =
    Decode.map HealthList <|
        Decode.list incomeDecoder
