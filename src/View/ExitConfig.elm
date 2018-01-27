module View.ExitConfig exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Data.ExitConfig as ExitConfig exposing (ExitConfig(..))
import Data.Tooltip as Tooltip
import Html exposing (Html, div, legend, text)
import Html.Attributes exposing (class, size)
import Html.Events exposing (onSubmit)
import Time.Date as Date exposing (Date)
import Types exposing (Msg(ExitConfigChanged, NoOp))
import Util
import View.Tooltip as Tooltip


form : ExitConfig -> Date -> Tooltip.States -> Card.BlockItem Msg
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
                    , Radio.onClick <| ExitConfigChanged <| ExitBy <| dateToString <| lastDayOfNextYear generatedOn
                    ]
                    "Opustit Zonky k "
                , Input.text
                    [ Input.small
                    , Input.disabled (exitEnum == Dont)
                    , Input.value exitDate
                    , Input.onInput (changeExitDate exitConfig)
                    , Input.attrs [ class "mx-1", size 10 ]
                    ]
                , Tooltip.popoverTip Tooltip.exitDateTip tooltipStates
                ]
            , if exitEnum /= Dont then
                leaveConfigSubform exitEnum exitDate selloffDate tooltipStates
              else
                text ""
            , validationErrors
            ]


{-| Generate last day of next year relative to current date
-}
lastDayOfNextYear : Date -> Date
lastDayOfNextYear generatedOn =
    generatedOn
        |> Date.setDay 31
        |> Date.setMonth 12
        |> Date.setYear (Date.year generatedOn + 1)


dateToString : Date -> String
dateToString =
    Date.toTuple >> (\( year, month, day ) -> toString day ++ "." ++ toString month ++ "." ++ toString year)


leaveConfigSubform : ExitConfigEnum -> String -> String -> Tooltip.States -> Html Msg
leaveConfigSubform exitEnum exitDate selloffDate tooltipStates =
    div [ class "mx-4" ]
        [ Form.formInline [ onSubmit NoOp ]
            [ Radio.radio
                [ Radio.checked (exitEnum == By)
                , Radio.name "selloffRadio"
                , Radio.onClick (ExitConfigChanged (ExitBy exitDate))
                ]
                ("Výprodej zahájit tři měsíce před datem opuštění " ++ parenthesizeNonempty (threeMonthsBeforeExitDate exitDate))
            , Tooltip.popoverTip Tooltip.sellofDateTip tooltipStates
            ]
        , Form.formInline [ onSubmit NoOp ]
            [ Radio.radio
                [ Radio.checked (exitEnum == WithSelloff)
                , Radio.name "selloffRadio"
                , Radio.onClick <| ExitConfigChanged <| ExitByWithSelloff exitDate <| threeMonthsBeforeExitDate exitDate
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


parenthesizeNonempty : String -> String
parenthesizeNonempty s =
    if String.isEmpty s then
        ""
    else
        "(" ++ s ++ ")"


threeMonthsBeforeExitDate : String -> String
threeMonthsBeforeExitDate exitDateStr =
    ExitConfig.parseDateString exitDateStr
        |> Result.map (Date.addMonths -3 >> dateToString)
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
