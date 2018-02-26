module View.TargetPortfolioSize exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize(NotSpecified, TargetPortfolioSize))
import Html exposing (text)
import Html.Attributes as Attr
import Html.Events exposing (onSubmit)
import Types exposing (Msg(NoOp, TargetPortfolioSizeChanged))
import Util


form : TargetPortfolioSize -> CardBlock.Item Msg
form targetPortfolioSize =
    let
        ( isUnbounded, valueAttribute ) =
            case targetPortfolioSize of
                NotSpecified ->
                    ( True, Input.value defaultSize )

                TargetPortfolioSize maxBound ->
                    ( False
                    , Input.value <| Util.zeroToEmpty maxBound
                    )

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
                    , Radio.checked isUnbounded
                    , Radio.name "portfolioSize"
                    , Radio.onClick (TargetPortfolioSizeChanged "undefined")
                    ]
                    "neomezená"
                , Radio.radio
                    [ Radio.id "tps2"
                    , Radio.checked (not isUnbounded)
                    , Radio.name "portfolioSize"
                    , Radio.onClick (TargetPortfolioSizeChanged defaultSize)
                    , Radio.attrs [ Spacing.mx1 ]
                    ]
                    "maximálně"
                , Input.number
                    [ Input.small
                    , Input.onInput TargetPortfolioSizeChanged
                    , Input.disabled isUnbounded
                    , valueAttribute
                    , Input.attrs [ Attr.min "0", Attr.max "100000000", Spacing.mx1 ]
                    ]
                , text "Kč."
                ]
            , validationErrors
            ]
        |> Fieldset.view
        |> CardBlock.custom


defaultSize : String
defaultSize =
    "10000"
