module View.ExitConfig exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Data.ExitConfig as ExitConfig exposing (ExitConfig(..))
import Html exposing (Html, div, legend, text)
import Html.Attributes exposing (class, size)
import Html.Events exposing (onSubmit)
import Time.Date exposing (Date)
import Types exposing (Msg(ExitConfigChanged, NoOp))
import Util


form : ExitConfig -> Date -> Card.BlockItem Msg
form exitConfig generatedOn =
    let
        validationErrors =
            Util.viewErrors <| ExitConfig.validate generatedOn exitConfig

        ( exitEnum, exitDate, selloffDate ) =
            case exitConfig of
                DontExit ->
                    ( Dont, "", "" )

                ExitBy date ->
                    ( By, date, "" )

                ExitByWithSelloff date1 date2 ->
                    ( WithSelloff, date1, date2 )
    in
    Card.custom <|
        Form.group []
            [ legend [] [ text "Opuštění Zonky" ]
            , Radio.radio
                [ Radio.checked (exitEnum == Dont)
                , Radio.name "exitConfig"
                , Radio.onClick (ExitConfigChanged DontExit)
                ]
                "Neopouštět Zonky"
            , Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.checked (exitEnum /= Dont)
                    , Radio.name "exitConfig"
                    , Radio.onClick (ExitConfigChanged (ExitBy ""))
                    ]
                    "Opustit Zonky k "
                , Input.text
                    [ Input.small
                    , Input.disabled (exitEnum == Dont)
                    , Input.value exitDate
                    , Input.onInput (changeExitDate exitConfig)
                    , Input.attrs [ class "mx-1", size 10 ]
                    ]
                ]
            , if exitEnum /= Dont then
                leaveConfigSubform exitEnum exitDate selloffDate
              else
                text ""
            , validationErrors
            ]


leaveConfigSubform : ExitConfigEnum -> String -> String -> Html Msg
leaveConfigSubform exitEnum exitDate selloffDate =
    div [ class "mx-4" ]
        [ Form.formInline [ onSubmit NoOp ]
            [ Radio.radio
                [ Radio.checked (exitEnum == By)
                , Radio.name "selloffRadio"
                , Radio.onClick (ExitConfigChanged (ExitBy exitDate))
                ]
                "Výprodej zahájit ihned"
            ]
        , Form.formInline [ onSubmit NoOp ]
            [ Radio.radio
                [ Radio.checked (exitEnum == WithSelloff)
                , Radio.name "selloffRadio"
                , Radio.onClick (ExitConfigChanged (ExitByWithSelloff exitDate ""))
                ]
                "Výprodej zahájit k"
            , Input.text
                [ Input.small
                , Input.disabled (exitEnum /= WithSelloff)
                , Input.value selloffDate
                , Input.onInput (ExitConfigChanged << ExitByWithSelloff exitDate)
                , Input.attrs [ class "mx-1", size 10 ]
                ]
            ]
        ]


type ExitConfigEnum
    = Dont
    | By
    | WithSelloff


changeExitDate : ExitConfig -> String -> Msg
changeExitDate exitConfig newExitDate =
    ExitConfigChanged <|
        case exitConfig of
            ExitBy _ ->
                ExitBy newExitDate

            ExitByWithSelloff _ selloff ->
                ExitByWithSelloff newExitDate selloff

            DontExit ->
                DontExit
