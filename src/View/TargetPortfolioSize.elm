module View.TargetPortfolioSize exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize(..))
import Html exposing (Html, text)
import Html.Events exposing (onSubmit)
import Types exposing (Msg(..))
import Util
import View.NumericInput


form : TargetPortfolioSize -> CardBlock.Item Msg
form targetPortfolioSize =
    let
        ( isBounded, value ) =
            case targetPortfolioSize of
                NotSpecified ->
                    ( False, defaultSize )

                TargetPortfolioSize maxBound ->
                    ( True, Util.zeroToEmpty maxBound )

        validationErrors =
            Util.viewErrors <| TargetPortfolioSize.validate targetPortfolioSize
    in
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ text "Cílová zůstatková částka" ]
        |> Fieldset.children
            [ Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.id "tps1"
                    , Radio.checked (not isBounded)
                    , Radio.name "portfolioSize"
                    , Radio.onClick (TargetPortfolioSizeChanged "undefined")
                    ]
                    "neomezená"
                , Radio.radio
                    [ Radio.id "tps2"
                    , Radio.checked isBounded
                    , Radio.name "portfolioSize"
                    , Radio.onClick (TargetPortfolioSizeChanged defaultSize)
                    , Radio.attrs [ Spacing.mx1 ]
                    ]
                    "maximálně"
                , numericInput TargetPortfolioSizeChanged isBounded value
                , text "Kč."
                ]
            , validationErrors
            ]
        |> Fieldset.view
        |> CardBlock.custom


numericInput : (String -> Msg) -> Bool -> String -> Html Msg
numericInput =
    View.NumericInput.numericInput 0 100000000


defaultSize : String
defaultSize =
    "10000"
