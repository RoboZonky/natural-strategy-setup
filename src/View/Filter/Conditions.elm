module View.Filter.Conditions exposing (Msg, conditionTypesThatApplyTo, form, getVisibleLabel, update)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Data.Filter exposing (FilteredItem(..))
import Data.Filter.Complexity exposing (FilterComplexity(..))
import Data.Filter.Conditions as C exposing (Condition(..), ConditionType(..), Conditions)
import Data.Filter.Conditions.Amount as Amount exposing (Amount(..), AmountCondition(..), AmountMsg)
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonths(..), ElapsedTermMonthsCondition(..), ElapsedTermMonthsMsg)
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercent(..), ElapsedTermPercentCondition(..), ElapsedTermPercentMsg)
import Data.Filter.Conditions.Income as Income exposing (Income(..), IncomeCondition(..), IncomeMsg)
import Data.Filter.Conditions.Insurance as Insurance exposing (Insurance(..), InsuranceCondition(..), InsuranceMsg)
import Data.Filter.Conditions.Interest as Interest exposing (Interest(..), InterestCondition(..), InterestMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (Purpose(..), PurposeCondition(..), PurposeMsg)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..), RatingCondition(..), RatingMsg)
import Data.Filter.Conditions.Region as Region exposing (Region(..), RegionCondition(..), RegionMsg)
import Data.Filter.Conditions.RemainingAmount as RemainingAmount exposing (RemainingAmount(..), RemainingAmountCondition(..), RemainingAmountMsg)
import Data.Filter.Conditions.Story as Story exposing (Story(..), StoryCondition(..), StoryMsg)
import Data.Filter.Conditions.TermMonths as TermMonths exposing (TermMonths(..), TermMonthsCondition(..), TermMonthsMsg)
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercent(..), TermPercentCondition(..), TermPercentMsg)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import View.EnumSelect as Select



-- MODEL


type alias Model =
    Conditions



-- VIEW


form : FilterComplexity -> FilteredItem -> Model -> Html Msg
form filterComplexity filteredItem conditions =
    let
        enabledConditions =
            C.getEnabledConditions conditions

        dropdownWhenFilterComplexOrConditionsEmpty =
            if {- Simple rules can only have one condition -} filterComplexity == Complex || List.isEmpty enabledConditions then
                conditionEnablementDropdown filteredItem conditions

            else
                text ""
    in
    div [ class "condition-subform-container" ]
        (List.map (conditionSubform filteredItem) enabledConditions
            ++ [ dropdownWhenFilterComplexOrConditionsEmpty ]
        )


conditionTypesThatApplyTo : FilteredItem -> List ConditionType
conditionTypesThatApplyTo filteredItem =
    let
        commonForAll =
            [ Rating, Interest, Purpose, Income, Story, Region, Term_Months, Insurance ]

        commonForParticipations =
            [ Term_Percent, Elapsed_Term_Months, Elapsed_Term_Percent, Remaining_Amount ]
    in
    case filteredItem of
        Loan ->
            Amount :: commonForAll

        Participation ->
            commonForParticipations ++ commonForAll

        Participation_To_Sell ->
            commonForParticipations ++ commonForAll

        Loan_And_Participation ->
            commonForAll


{-| Dropdown for enabling conditionts that are currently disabled, but can be enabled for given FilteredItem
-}
conditionEnablementDropdown : FilteredItem -> Conditions -> Html Msg
conditionEnablementDropdown filteredItem conditions =
    let
        validConditions =
            conditionTypesThatApplyTo filteredItem

        conditionsThatCanBeEnabled =
            List.filter (\c -> List.member c validConditions) <| C.getDisabledConditionTypes conditions

        createConditionEnablingMessage conditionType =
            AddCondition <| C.getDefaultCondition conditionType
    in
    if List.isEmpty conditionsThatCanBeEnabled then
        text ""

    else
        Select.from
            { enumValues = conditionsThatCanBeEnabled
            , valuePickedMessage = createConditionEnablingMessage
            , showVisibleLabel = getVisibleLabel filteredItem
            , dummyOption = "-- Přidat podmínku --"
            }


termConditionLabel : FilteredItem -> String -> String
termConditionLabel filteredItem unitStr =
    case filteredItem of
        Loan ->
            "Délka úvěru " ++ unitStr

        _ ->
            "Zbývající délka úvěru " ++ unitStr


conditionSubform : FilteredItem -> Condition -> Html Msg
conditionSubform item condition =
    let
        wrap =
            closeableWrapper item
    in
    case condition of
        Condition_Amount c ->
            wrap Amount (Html.map AmountMsg <| Amount.form c)

        Condition_Elapsed_Term_Months c ->
            wrap Elapsed_Term_Months (Html.map ElapsedTermMonthsMsg <| ElapsedTermMonths.form c)

        Condition_Elapsed_Term_Percent c ->
            wrap Elapsed_Term_Percent (Html.map ElapsedTermPercentMsg <| ElapsedTermPercent.form c)

        Condition_Income c ->
            wrap Income (Html.map IncomeMsg <| Income.form c)

        Condition_Insurance c ->
            wrap Insurance (Html.map InsuranceMsg <| Insurance.form c)

        Condition_Interest c ->
            wrap Interest (Html.map InterestMsg <| Interest.form c)

        Condition_Purpose c ->
            wrap Purpose (Html.map PurposeMsg <| Purpose.form c)

        Condition_Rating c ->
            wrap Rating (Html.map RatingMsg <| Rating.form "rating_" c)

        Condition_Region c ->
            wrap Region (Html.map RegionMsg <| Region.form c)

        Condition_Remaining_Amount c ->
            wrap Remaining_Amount (Html.map RemainingAmountMsg <| RemainingAmount.form c)

        Condition_Story c ->
            wrap Story (Html.map StoryMsg <| Story.form c)

        Condition_Term_Months c ->
            wrap Term_Months (Html.map TermMonthsMsg <| TermMonths.form c)

        Condition_Term_Percent c ->
            wrap Term_Percent (Html.map TermPercentMsg <| TermPercent.form c)


getVisibleLabel : FilteredItem -> ConditionType -> String
getVisibleLabel filteredItem conditionType =
    case conditionType of
        Amount ->
            "Výše úvěru"

        Elapsed_Term_Months ->
            -- Note that this string contains &nbsp; entered by pasting escape sequence "\x00A0"
            -- (which is then transformed into corresponding unicode char by elm-format)
            "Uhrazeno splátek (v\u{00A0}měsících)"

        --
        Elapsed_Term_Percent ->
            "Uhrazeno splátek (v\u{00A0}%)"

        Income ->
            "Zdroj příjmů klienta"

        Insurance ->
            "Pojištění"

        Interest ->
            "Úrok"

        Purpose ->
            "Účel úvěru"

        Rating ->
            "Rating"

        Region ->
            "Kraj klienta"

        Remaining_Amount ->
            "Zbývající jistina"

        Story ->
            "Příběh"

        Term_Months ->
            termConditionLabel filteredItem "(v\u{00A0}měsících)"

        Term_Percent ->
            termConditionLabel filteredItem "(v\u{00A0}%)"


closeableWrapper : FilteredItem -> ConditionType -> Html Msg -> Html Msg
closeableWrapper filteredItem conditionType subform =
    let
        removeButton =
            span [ onClick (RemoveCondition conditionType), class "float-right" ] [ text "✖" ]

        conditionLabel =
            text <| getVisibleLabel filteredItem conditionType
    in
    Grid.row [ Row.attrs [ class "condition-subform" ] ]
        [ Grid.col [ Col.xs3 ] [ conditionLabel ]
        , Grid.col [ Col.xs8 ] [ subform ]
        , Grid.col [ Col.xs1 ] [ removeButton ]
        ]



-- UPDATE


type
    Msg
    -- Forwarding messages to individual condition subforms
    = AmountMsg AmountMsg
    | ElapsedTermMonthsMsg ElapsedTermMonthsMsg
    | ElapsedTermPercentMsg ElapsedTermPercentMsg
    | InsuranceMsg InsuranceMsg
    | InterestMsg InterestMsg
    | IncomeMsg IncomeMsg
    | PurposeMsg PurposeMsg
    | RatingMsg RatingMsg
    | RegionMsg RegionMsg
    | RemainingAmountMsg RemainingAmountMsg
    | StoryMsg StoryMsg
    | TermMonthsMsg TermMonthsMsg
    | TermPercentMsg TermPercentMsg
      -- Control enabling / disabling conditions
    | AddCondition Condition
    | RemoveCondition ConditionType


update : Msg -> Model -> Model
update msg model =
    case msg of
        AmountMsg m ->
            C.updateAmount m model

        ElapsedTermMonthsMsg m ->
            C.updateElapsedTermMonths m model

        ElapsedTermPercentMsg m ->
            C.updateElapsedTermPercent m model

        IncomeMsg m ->
            C.updateIncome m model

        InsuranceMsg m ->
            C.updateInsurance m model

        InterestMsg m ->
            C.updateInterest m model

        PurposeMsg m ->
            C.updatePurpose m model

        RatingMsg m ->
            C.updateRating m model

        RegionMsg m ->
            C.updateRegion m model

        RemainingAmountMsg m ->
            C.updateRemainingAmount m model

        StoryMsg m ->
            C.updateStory m model

        TermMonthsMsg m ->
            C.updateTermMonths m model

        TermPercentMsg m ->
            C.updateTermPercent m model

        AddCondition c ->
            C.addCondition c model

        RemoveCondition ct ->
            C.removeCondition ct model
