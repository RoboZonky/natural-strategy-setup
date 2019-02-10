module Test.TestApp exposing (main)

import Browser
import Data.Strategy as Strategy exposing (StrategyConfiguration)
import Data.VersionedStrategy as VersioinedStrategy
import Html exposing (Html, button, div, input, span, text, textarea)
import Html.Attributes exposing (cols, id, readonly, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import Test.RandomStrategy as RandomStrategy
import Time exposing (Posix)
import Util


type Msg
    = PrevSeed
    | SetSeed String
    | NextSeed


type alias Model =
    Int


main : Program () Model Msg
main =
    Browser.sandbox
        { init = 1
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg curSeed =
    case msg of
        PrevSeed ->
            curSeed - 1

        NextSeed ->
            curSeed + 1

        SetSeed newSeed ->
            String.toInt newSeed |> Maybe.withDefault 0


view : Model -> Html Msg
view seed =
    let
        dummyGeneratedOn =
            Time.millisToPosix 0

        ( randomStrategyConfig, _ ) =
            Random.step RandomStrategy.strategyConfigurationGen (Random.initialSeed seed)

        strategyString =
            Strategy.renderStrategyConfiguration "dummy" dummyGeneratedOn randomStrategyConfig
    in
    div []
        [ textarea
            [ value strategyString
            , readonly True
            , rows 50
            , cols 150
            , id "renderedStrategy"
            ]
            []
        , div []
            [ button [ onClick PrevSeed ] [ text "Previous Seed" ]
            , input [ onInput SetSeed, type_ "text", value (String.fromInt seed), id "seed" ] []
            , button [ onClick NextSeed, id "nextSeedButton" ] [ text "Next Seed" ]
            ]
        , div []
            [ text <| "Strategy validation errors: "
            , span [ id "validationErrors" ] [ text <| Util.stringListToString <| Strategy.validateStrategyConfiguration randomStrategyConfig ]
            ]
        , div []
            [ text "JSON encode / decode roundtrip: "
            , span [ id "encodingDecodingResult" ] [ text <| testEncodeDecodeDoesntChangeStrategy randomStrategyConfig ]
            ]
        ]


testEncodeDecodeDoesntChangeStrategy : StrategyConfiguration -> String
testEncodeDecodeDoesntChangeStrategy original =
    case encodeDecodeStrategy original of
        Ok decoded ->
            if Strategy.strategyEqual original decoded then
                "Ok"

            else
                String.join "\n"
                    [ "Decoded strategy is different from original"
                    , "---------- ORIGINAL ----------"
                    , showStrategy original
                    , "---------- DECODED ----------"
                    , showStrategy decoded
                    ]

        Err e ->
            "Failed to decode strategy : " ++ e


encodeDecodeStrategy : StrategyConfiguration -> Result String StrategyConfiguration
encodeDecodeStrategy strategy =
    Strategy.strategyToUrlHash strategy
        |> VersioinedStrategy.loadStrategy
        |> Result.map Tuple.first


epoch : Posix
epoch =
    Time.millisToPosix 0


showStrategy : StrategyConfiguration -> String
showStrategy =
    Strategy.renderStrategyConfiguration "DUMMY_URL" epoch
