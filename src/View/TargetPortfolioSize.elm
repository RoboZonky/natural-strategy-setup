module View.TargetPortfolioSize exposing
    ( Config
    , form
    )

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize(..))
import Html exposing (Html)
import Html.Events exposing (onSubmit)
import Util
import View.NumericInput


type alias Config msg =
    { targetPortfolioSizeChanged : String -> msg
    , noOp : msg
    }


form : Config msg -> TargetPortfolioSize -> CardBlock.Item msg
form config targetPortfolioSize =
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
        |> Fieldset.legend [] [ Html.text "Cílová zůstatková částka" ]
        |> Fieldset.children
            [ Form.formInline [ onSubmit config.noOp ]
                [ Radio.radio
                    [ Radio.id "tps1"
                    , Radio.checked (not isBounded)
                    , Radio.name "portfolioSize"
                    , Radio.onClick (config.targetPortfolioSizeChanged "undefined")
                    ]
                    "neomezená"
                , Radio.radio
                    [ Radio.id "tps2"
                    , Radio.checked isBounded
                    , Radio.name "portfolioSize"
                    , Radio.onClick (config.targetPortfolioSizeChanged defaultSize)
                    , Radio.attrs [ Spacing.mx1 ]
                    ]
                    "maximálně"
                , numericInput config.targetPortfolioSizeChanged isBounded value
                , Html.text "Kč."
                ]
            , validationErrors
            ]
        |> Fieldset.view
        |> CardBlock.custom


numericInput : (String -> msg) -> Bool -> String -> Html msg
numericInput =
    View.NumericInput.numericInput 0 100000000


defaultSize : String
defaultSize =
    "10000"
