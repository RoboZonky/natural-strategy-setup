module View.ExitConfig exposing (Config, form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import Data.ExitConfig as ExitConfig exposing (ExitConfig(..))
import Data.Tooltip as Tooltip
import Html exposing (Html)
import Html.Attributes exposing (size)
import Html.Events exposing (onSubmit)
import Time exposing (Posix)
import Time.Extra as TE
import Util
import Version
import View.Tooltip as Tooltip


type alias Config msg =
    { exitConfigChanged : ExitConfig -> msg
    , tooltipConfig : Tooltip.Config msg
    , noOp : msg
    }


form : Config msg -> ExitConfig -> Posix -> Tooltip.States -> CardBlock.Item msg
form config exitConfig generatedOn tooltipStates =
    let
        validationErrors =
            Util.viewErrors <| ExitConfig.validate exitConfig

        ( exitEnum, exitDate, selloffDate ) =
            case exitConfig of
                DontExit ->
                    ( Dont, dateToString (lastDayOfNextYear generatedOn) {- show default pre-filled in disabled field -}, "" )

                ExitBy date ->
                    ( By, date, threeMonthsBeforeExitDate date {- show default pre-filled in disabled field -} )

                ExitByWithSelloff date1 date2 ->
                    ( WithSelloff, date1, date2 )
    in
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ Html.text "Opuštění Zonky" ]
        |> Fieldset.children
            [ Radio.radio
                [ Radio.id "exit1"
                , Radio.checked (exitEnum == Dont)
                , Radio.name "exitConfig"
                , Radio.onClick (config.exitConfigChanged DontExit)
                ]
                "Neopouštět Zonky"
            , Form.formInline [ onSubmit config.noOp ]
                [ Radio.radio
                    [ Radio.id "exit2"
                    , Radio.checked (exitEnum /= Dont)
                    , Radio.name "exitConfig"
                    , Radio.onClick <| config.exitConfigChanged <| ExitBy <| dateToString <| lastDayOfNextYear generatedOn
                    ]
                    "Opustit Zonky k "
                , Input.text
                    [ Input.small
                    , Input.disabled (exitEnum == Dont)
                    , Input.value exitDate
                    , Input.onInput (config.exitConfigChanged << changeExitDate exitConfig)
                    , Input.attrs [ Spacing.mx1, size 10 ]
                    ]
                , Tooltip.popoverTip config.tooltipConfig Tooltip.exitDateTip tooltipStates
                ]
            , if exitEnum /= Dont then
                leaveConfigSubForm config exitEnum exitDate selloffDate tooltipStates

              else
                Html.text ""
            , validationErrors
            ]
        |> Fieldset.view
        |> CardBlock.custom


{-| Generate last day of next year relative to current date
-}
lastDayOfNextYear : Posix -> Posix
lastDayOfNextYear generatedOn =
    TE.ceiling TE.Year Time.utc generatedOn
        |> TE.add TE.Year 1 Time.utc


dateToString : Posix -> String
dateToString =
    Version.formatDate


leaveConfigSubForm : Config msg -> ExitConfigEnum -> String -> String -> Tooltip.States -> Html msg
leaveConfigSubForm config exitEnum exitDate selloffDate tooltipStates =
    Html.div [ Spacing.mx5 ]
        [ Form.formInline [ onSubmit config.noOp ]
            [ Radio.radio
                [ Radio.id "ex3"
                , Radio.checked (exitEnum == By)
                , Radio.name "selloffRadio"
                , Radio.onClick <| config.exitConfigChanged <| ExitBy exitDate
                ]
                ("Výprodej zahájit tři měsíce před datem opuštění " ++ parenthesizeNonempty (threeMonthsBeforeExitDate exitDate))
            , Tooltip.popoverTip config.tooltipConfig Tooltip.sellofDateTip tooltipStates
            ]
        , Form.formInline [ onSubmit config.noOp ]
            [ Radio.radio
                [ Radio.id "ex4"
                , Radio.checked (exitEnum == WithSelloff)
                , Radio.name "selloffRadio"
                , Radio.onClick <| config.exitConfigChanged <| ExitByWithSelloff exitDate <| threeMonthsBeforeExitDate exitDate
                ]
                "Výprodej zahájit k"
            , Input.text
                [ Input.small
                , Input.disabled (exitEnum /= WithSelloff)
                , Input.value selloffDate
                , Input.onInput (config.exitConfigChanged << ExitByWithSelloff exitDate)
                , Input.attrs [ Spacing.mx1, size 10 ]
                ]
            ]
        ]


parenthesizeNonempty : String -> String
parenthesizeNonempty s =
    if String.isEmpty s then
        ""

    else
        "(" ++ s ++ ")"


threeMonthsBeforeExitDate : String -> String
threeMonthsBeforeExitDate exitDateStr =
    ExitConfig.parseDateString exitDateStr
        |> Result.map (TE.add TE.Month -3 Time.utc >> dateToString)
        |> Result.withDefault ""


type ExitConfigEnum
    = Dont
    | By
    | WithSelloff


changeExitDate : ExitConfig -> String -> ExitConfig
changeExitDate exitConfig newExitDate =
    case exitConfig of
        ExitBy _ ->
            ExitBy newExitDate

        ExitByWithSelloff _ selloff ->
            ExitByWithSelloff newExitDate selloff

        DontExit ->
            DontExit
