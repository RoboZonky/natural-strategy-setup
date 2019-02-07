module View.ExitConfig exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import Data.ExitConfig as ExitConfig exposing (ExitConfig(..))
import Data.Tooltip as Tooltip
import Html exposing (Html, div, text)
import Html.Attributes exposing (size)
import Html.Events exposing (onSubmit)
import Time exposing (Posix)
import Time.Extra as TE
import Types exposing (Msg(..))
import Util
import Version
import View.Tooltip as Tooltip


form : ExitConfig -> Posix -> Tooltip.States -> CardBlock.Item Msg
form exitConfig generatedOn tooltipStates =
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
        |> Fieldset.legend [] [ text "Opuštění Zonky" ]
        |> Fieldset.children
            [ Radio.radio
                [ Radio.id "exit1"
                , Radio.checked (exitEnum == Dont)
                , Radio.name "exitConfig"
                , Radio.onClick (ExitConfigChanged DontExit)
                ]
                "Neopouštět Zonky"
            , Form.formInline [ onSubmit NoOp ]
                [ Radio.radio
                    [ Radio.id "exit2"
                    , Radio.checked (exitEnum /= Dont)
                    , Radio.name "exitConfig"
                    , Radio.onClick <| ExitConfigChanged <| ExitBy <| dateToString <| lastDayOfNextYear generatedOn
                    ]
                    "Opustit Zonky k "
                , Input.text
                    [ Input.small
                    , Input.disabled (exitEnum == Dont)
                    , Input.value exitDate
                    , Input.onInput (changeExitDate exitConfig)
                    , Input.attrs [ Spacing.mx1, size 10 ]
                    ]
                , Tooltip.popoverTip Tooltip.exitDateTip tooltipStates
                ]
            , if exitEnum /= Dont then
                leaveConfigSubform exitEnum exitDate selloffDate tooltipStates

              else
                text ""
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


leaveConfigSubform : ExitConfigEnum -> String -> String -> Tooltip.States -> Html Msg
leaveConfigSubform exitEnum exitDate selloffDate tooltipStates =
    div [ Spacing.mx4 ]
        [ Form.formInline [ onSubmit NoOp ]
            [ Radio.radio
                [ Radio.id "ex3"
                , Radio.checked (exitEnum == By)
                , Radio.name "selloffRadio"
                , Radio.onClick (ExitConfigChanged (ExitBy exitDate))
                ]
                ("Výprodej zahájit tři měsíce před datem opuštění " ++ parenthesizeNonempty (threeMonthsBeforeExitDate exitDate))
            , Tooltip.popoverTip Tooltip.sellofDateTip tooltipStates
            ]
        , Form.formInline [ onSubmit NoOp ]
            [ Radio.radio
                [ Radio.id "ex4"
                , Radio.checked (exitEnum == WithSelloff)
                , Radio.name "selloffRadio"
                , Radio.onClick <| ExitConfigChanged <| ExitByWithSelloff exitDate <| threeMonthsBeforeExitDate exitDate
                ]
                "Výprodej zahájit k"
            , Input.text
                [ Input.small
                , Input.disabled (exitEnum /= WithSelloff)
                , Input.value selloffDate
                , Input.onInput (ExitConfigChanged << ExitByWithSelloff exitDate)
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
