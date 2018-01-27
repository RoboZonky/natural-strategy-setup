module Test.TestApp exposing (main)

import Data.Strategy as Strategy
import Html exposing (Html, button, div, input, span, text, textarea)
import Html.Attributes exposing (cols, id, readonly, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import Test.RandomStrategy as RandomStrategy
import Time.DateTime as DateTime


type Msg
    = PrevSeed
    | SetSeed String
    | NextSeed


type alias Model =
    Int


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = 1, view = view, update = update }


update : Msg -> Model -> Model
update msg curSeed =
    case msg of
        PrevSeed ->
            curSeed - 1

        NextSeed ->
            curSeed + 1

        SetSeed newSeed ->
            String.toInt newSeed |> Result.withDefault 0


view : Model -> Html Msg
view seed =
    let
        dummyGeneratedOn =
            DateTime.date DateTime.epoch

        ( randomStrategyConfig, _ ) =
            Random.step RandomStrategy.strategyConfigurationGen (Random.initialSeed seed)

        strategyString =
            Strategy.renderStrategyConfiguration dummyGeneratedOn randomStrategyConfig
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
            , input [ onInput SetSeed, type_ "text", value (toString seed), id "seed" ] []
            , button [ onClick NextSeed, id "nextSeedButton" ] [ text "Next Seed" ]
            ]
        , div []
            [ text <| "Strategy validation errors: "
            , span [ id "validationErrors" ] [ text <| toString <| Strategy.validateStrategyConfiguration randomStrategyConfig ]
            ]
        ]
