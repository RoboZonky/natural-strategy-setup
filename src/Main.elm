module Main exposing (..)

import Html exposing (Html, text)
import Data.Strategy exposing (..)
import Types exposing (..)
import View.StrategyForm as StrategyForm
import View.ConfigPreview as ConfigPreview
import Data.Strategy.Portfolio as Portfolio


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( SimpleStrategy Portfolio.Conservative
            , Cmd.none
            )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SimpleStrategySelected ->
            ( defaultSimpleStrategy, Cmd.none )

        ComplexStrategySelected ->
            ( defaultComplexStrategy, Cmd.none )

        PortfolioChanged portfolio ->
            ( setPortfolio portfolio model, Cmd.none )

        TargetPortfolioSizeChanged targetSizeStr ->
            let
                targetSize =
                    case String.toInt targetSizeStr of
                        Ok sz ->
                            Bounded sz

                        Err error ->
                            Unbounded
            in
                ( setTargetPortfolioSize targetSize model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ StrategyForm.view model
        , ConfigPreview.view model
        ]
