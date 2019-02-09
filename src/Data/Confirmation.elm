module Data.Confirmation exposing
    ( ConfirmationFormMsg(..)
    , ConfirmationSettings(..)
    , confirmationsDisabled
    , decoder
    , encode
    , equal
    , render
    , update
    , validate
    )

import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type ConfirmationFormMsg
    = DisableConfirmation
    | EnableConfirmation
    | UpdateConfirmation Interest.InterestMsg


type ConfirmationSettings
    = NoConfirmation
    | Confirm InterestCondition


confirmationsDisabled : ConfirmationSettings
confirmationsDisabled =
    NoConfirmation


update : ConfirmationFormMsg -> ConfirmationSettings -> ConfirmationSettings
update msg settings =
    case msg of
        DisableConfirmation ->
            NoConfirmation

        EnableConfirmation ->
            Confirm Interest.defaultCondition

        UpdateConfirmation interestMsg ->
            case settings of
                Confirm interestCondition ->
                    Confirm (Interest.update interestMsg interestCondition)

                NoConfirmation ->
                    Confirm (Interest.update interestMsg Interest.defaultCondition)


render : ConfirmationSettings -> String
render settings =
    case settings of
        NoConfirmation ->
            ""

        Confirm interestCondition ->
            "Potvrzovat mobilem investice do úvěrů, kde " ++ Interest.renderCondition interestCondition ++ "."


validate : ConfirmationSettings -> List String
validate settings =
    case settings of
        NoConfirmation ->
            []

        Confirm interestCondition ->
            List.map (\err -> "Potvrzení investic mobilem: " ++ err) <| Interest.validationErrors interestCondition


equal : ConfirmationSettings -> ConfirmationSettings -> Bool
equal cs1 cs2 =
    cs1 == cs2



--JSON


encode : ConfirmationSettings -> Value
encode cs =
    case cs of
        NoConfirmation ->
            Encode.object
                [ ( "a", Encode.int 0 ) ]

        Confirm interestCondition ->
            Encode.object
                [ ( "a", Encode.int 1 )
                , ( "b", Interest.encodeCondition interestCondition )
                ]


decoder : Decoder ConfirmationSettings
decoder =
    Decode.field "a" Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    0 ->
                        Decode.succeed NoConfirmation

                    1 ->
                        Decode.map Confirm <| Decode.field "b" Interest.conditionDecoder

                    other ->
                        Decode.fail <| "Failed to decode ConfirmationSettings. Was expecting 0 or 1, but got " ++ String.fromInt other
            )
