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
import Data.Filter.Conditions.LoanAnnuity as LoanAnnuity exposing (LoanAnnuity(..), LoanAnnuityCondition(..), LoanAnnuityMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (Purpose(..), PurposeCondition(..), PurposeMsg)
import Data.Filter.Conditions.Region as Region exposing (Region(..), RegionCondition(..), RegionMsg)
import Data.Filter.Conditions.RemainingAmount as RemainingAmount exposing (RemainingAmount(..), RemainingAmountCondition(..), RemainingAmountMsg)
import Data.Filter.Conditions.RevenueRate as RevenueRate exposing (RevenueRate(..), RevenueRateCondition(..), RevenueRateMsg)
import Data.Filter.Conditions.SaleFee as SaleFee exposing (SaleFee(..), SaleFeeCondition(..), SaleFeeMsg(..))
import Data.Filter.Conditions.Story as Story exposing (Story(..), StoryCondition(..), StoryMsg)
import Data.Filter.Conditions.TermMonths as TermMonths exposing (TermMonths(..), TermMonthsCondition(..), TermMonthsMsg)
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercent(..), TermPercentCondition(..), TermPercentMsg)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import View.EnumSelect as Select exposing (DefaultOptionConfig(..))



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
            [ Amount, Interest, Purpose, Income, Story, Region, Term_Months, Insurance, Loan_Annuity, Revenue_Rate ]

        commonForParticipations =
            [ Term_Percent, Elapsed_Term_Months, Elapsed_Term_Percent, Remaining_Amount ]
    in
    commonForAll
        ++ (case filteredItem of
                Loan ->
                    []

                Loan_And_Participation ->
                    []

                Participation ->
                    commonForParticipations

                Participation_To_Sell ->
                    Sale_Fee :: commonForParticipations
           )


{-| Dropdown for enabling conditions that are currently disabled, but can be enabled for given FilteredItem
-}
conditionEnablementDropdown : FilteredItem -> Conditions -> Html Msg
conditionEnablementDropdown filteredItem conditions =
    let
        validConditions =
            conditionTypesThatApplyTo filteredItem

        conditionsThatCanBeEnabled =
            List.filter (\c -> List.member c validConditions) <| C.getDisabledConditionTypes conditions

        addCondition conditionType =
            AddCondition <| C.getDefaultCondition conditionType
    in
    if List.isEmpty conditionsThatCanBeEnabled then
        text ""

    else
        Select.from
            { enumValues = conditionsThatCanBeEnabled
            , valuePickedMessage = addCondition
            , showVisibleLabel = getVisibleLabel filteredItem
            , defaultOption = DummyOption "-- Přidat podmínku --"
            , enabled = True
            }


conditionSubform : FilteredItem -> Condition -> Html Msg
conditionSubform item condition =
    let
        wrap =
            closeableWrapper item
    in
    case condition of
        Condition_Amount c ->
            wrap Amount <| Html.map AmountMsg <| Amount.form c

        Condition_Elapsed_Term_Months c ->
            wrap Elapsed_Term_Months <| Html.map ElapsedTermMonthsMsg <| ElapsedTermMonths.form c

        Condition_Elapsed_Term_Percent c ->
            wrap Elapsed_Term_Percent <| Html.map ElapsedTermPercentMsg <| ElapsedTermPercent.form c

        Condition_Income c ->
            wrap Income <| Html.map IncomeMsg <| Income.form c

        Condition_Insurance c ->
            wrap Insurance <| Html.map InsuranceMsg <| Insurance.form c

        Condition_Interest c ->
            wrap Interest <| Html.map InterestMsg <| Interest.form c

        Condition_Loan_Annuity c ->
            wrap Loan_Annuity <| Html.map LoanAnnuityMsg <| LoanAnnuity.form c

        Condition_Purpose c ->
            wrap Purpose <| Html.map PurposeMsg <| Purpose.form c

        Condition_Region c ->
            wrap Region <| Html.map RegionMsg <| Region.form c

        Condition_Remaining_Amount c ->
            wrap Remaining_Amount <| Html.map RemainingAmountMsg <| RemainingAmount.form c

        Condition_Revenue_Rate c ->
            wrap Revenue_Rate <| Html.map RevenueRateMsg <| RevenueRate.form c

        Condition_Sale_Fee c ->
            wrap Sale_Fee <| Html.map SaleFeeMsg <| SaleFee.form c

        Condition_Story c ->
            wrap Story <| Html.map StoryMsg <| Story.form c

        Condition_Term_Months c ->
            wrap Term_Months <| Html.map TermMonthsMsg <| TermMonths.form c

        Condition_Term_Percent c ->
            wrap Term_Percent <| Html.map TermPercentMsg <| TermPercent.form c


getVisibleLabel : FilteredItem -> ConditionType -> String
getVisibleLabel filteredItem conditionType =
    case conditionType of
        Amount ->
            amountConditionLabel filteredItem

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

        Loan_Annuity ->
            "Měsíční splátka"

        Purpose ->
            "Účel úvěru"

        Region ->
            "Kraj klienta"

        Remaining_Amount ->
            "Zbývající jistina"

        Revenue_Rate ->
            "Optimální výnos"

        Sale_Fee ->
            "Poplatek za prodej"

        Story ->
            "Příběh"

        Term_Months ->
            termConditionLabel filteredItem "(v\u{00A0}měsících)"

        Term_Percent ->
            termConditionLabel filteredItem "(v\u{00A0}%)"


termConditionLabel : FilteredItem -> String -> String
termConditionLabel filteredItem unitStr =
    case filteredItem of
        Loan ->
            "Délka úvěru " ++ unitStr

        _ ->
            "Zbývající délka úvěru " ++ unitStr


amountConditionLabel : FilteredItem -> String
amountConditionLabel filteredItem =
    case filteredItem of
        Loan ->
            "Výše úvěru"

        _ ->
            "Původní výše úvěru"


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
    | LoanAnnuityMsg LoanAnnuityMsg
    | PurposeMsg PurposeMsg
    | RegionMsg RegionMsg
    | RemainingAmountMsg RemainingAmountMsg
    | RevenueRateMsg RevenueRateMsg
    | SaleFeeMsg SaleFeeMsg
    | StoryMsg StoryMsg
    | TermMonthsMsg TermMonthsMsg
    | TermPercentMsg TermPercentMsg
      -- Control enabling / disabling conditions
    | AddCondition Condition
    | RemoveCondition ConditionType


update : Msg -> Model -> Model
update msg =
    case msg of
        AmountMsg m ->
            C.updateAmount m

        ElapsedTermMonthsMsg m ->
            C.updateElapsedTermMonths m

        ElapsedTermPercentMsg m ->
            C.updateElapsedTermPercent m

        IncomeMsg m ->
            C.updateIncome m

        InsuranceMsg m ->
            C.updateInsurance m

        InterestMsg m ->
            C.updateInterest m

        LoanAnnuityMsg m ->
            C.updateLoanAnnuity m

        PurposeMsg m ->
            C.updatePurpose m

        RegionMsg m ->
            C.updateRegion m

        RemainingAmountMsg m ->
            C.updateRemainingAmount m

        RevenueRateMsg m ->
            C.updateRevenueRate m

        SaleFeeMsg m ->
            C.updateSaleFee m

        StoryMsg m ->
            C.updateStory m

        TermMonthsMsg m ->
            C.updateTermMonths m

        TermPercentMsg m ->
            C.updateTermPercent m

        AddCondition c ->
            C.addCondition c

        RemoveCondition ct ->
            C.removeCondition ct
