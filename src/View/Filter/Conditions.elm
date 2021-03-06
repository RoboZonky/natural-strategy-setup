module View.Filter.Conditions exposing (Msg, conditionTypesThatApplyTo, form, getVisibleLabel, update)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Data.Filter exposing (FilteredItem(..))
import Data.Filter.Complexity exposing (FilterComplexity(..))
import Data.Filter.Conditions as Conditions exposing (Condition(..), ConditionType(..), Conditions, processCondition)
import Data.Filter.Conditions.Amount as Amount exposing (Amount(..), AmountCondition(..), AmountMsg)
import Data.Filter.Conditions.CurrentDaysPastDue as CurrentDaysPastDue exposing (CurrentDaysPastDueMsg)
import Data.Filter.Conditions.DaysSinceLastPastDue as DaysSinceLastPastDue exposing (DaysSinceLastPastDueMsg)
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonths(..), ElapsedTermMonthsCondition(..), ElapsedTermMonthsMsg)
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercent(..), ElapsedTermPercentCondition(..), ElapsedTermPercentMsg)
import Data.Filter.Conditions.Health as Health exposing (HealthMsg)
import Data.Filter.Conditions.Income as Income exposing (Income, IncomeMsg)
import Data.Filter.Conditions.Insurance as Insurance exposing (Insurance, InsuranceCondition)
import Data.Filter.Conditions.Interest as Interest exposing (Interest(..), InterestCondition(..), InterestMsg)
import Data.Filter.Conditions.LoanAnnuity as LoanAnnuity exposing (LoanAnnuity(..), LoanAnnuityCondition(..), LoanAnnuityMsg)
import Data.Filter.Conditions.LongestDaysPastDue as LongestDaysPastDue exposing (LongestDaysPastDueMsg)
import Data.Filter.Conditions.OriginalTermMonths as OriginalTermMonths exposing (OriginalTermMonthsMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (Purpose, PurposeMsg)
import Data.Filter.Conditions.Region as Region exposing (Region, RegionMsg)
import Data.Filter.Conditions.RelativeProfit as RelativeProfit exposing (RelativeProfitMsg)
import Data.Filter.Conditions.RelativeSaleDiscount as RelativeSaleDiscount exposing (RelativeSaleDiscountMsg)
import Data.Filter.Conditions.RemainingAmount as RemainingAmount exposing (RemainingAmount(..), RemainingAmountCondition(..), RemainingAmountMsg)
import Data.Filter.Conditions.RemainingTermMonths as RemainingTermMonths exposing (RemainingTermMonths(..), RemainingTermMonthsCondition(..), RemainingTermMonthsMsg)
import Data.Filter.Conditions.RevenueRate as RevenueRate exposing (RevenueRate(..), RevenueRateCondition(..), RevenueRateMsg)
import Data.Filter.Conditions.SaleFee as SaleFee exposing (SaleFeeCondition)
import Data.Filter.Conditions.Story as Story exposing (Story, StoryCondition)
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercent(..), TermPercentCondition(..), TermPercentMsg)
import Html exposing (Html)
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
            Conditions.getEnabledConditions conditions

        dropDownWhenFilterComplexOrConditionsEmpty =
            if {- Simple rules can only have one condition -} filterComplexity == Complex || List.isEmpty enabledConditions then
                conditionEnablementDropDown filteredItem conditions

            else
                Html.text ""
    in
    Html.div [ class "condition-subform-container" ]
        (List.map (conditionSubForm filteredItem) enabledConditions
            ++ [ dropDownWhenFilterComplexOrConditionsEmpty ]
        )


conditionTypesThatApplyTo : FilteredItem -> List ConditionType
conditionTypesThatApplyTo filteredItem =
    let
        commonForAll =
            [ Amount
            , Interest
            , Purpose
            , Income
            , Story
            , Region
            , Remaining_Term_Months
            , Insurance
            , Loan_Annuity
            , Revenue_Rate
            ]

        commonForParticipations =
            [ Term_Percent
            , Elapsed_Term_Months
            , Elapsed_Term_Percent
            , Remaining_Amount
            , Health
            , Original_Term_Months
            , Relative_Sale_Discount
            , Current_Days_Past_Due
            , Days_Since_Last_Past_Due
            , Longest_Days_Past_Due
            ]
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
                    Relative_Profit :: Sale_Fee :: commonForParticipations
           )


{-| DropDown for enabling conditions that are currently disabled, but can be enabled for given FilteredItem
-}
conditionEnablementDropDown : FilteredItem -> Conditions -> Html Msg
conditionEnablementDropDown filteredItem conditions =
    let
        validConditions =
            conditionTypesThatApplyTo filteredItem

        conditionsThatCanBeEnabled =
            List.filter (\c -> List.member c validConditions) <| Conditions.getDisabledConditionTypes conditions

        addCondition conditionType =
            AddCondition <| Conditions.getDefaultCondition conditionType

        getLabel : ConditionType -> String
        getLabel =
            getVisibleLabel filteredItem

        groupAndLabel : ConditionType -> ( String, String )
        groupAndLabel ct =
            ( Conditions.getCategory ct, getLabel ct )
    in
    if List.isEmpty conditionsThatCanBeEnabled then
        Html.text ""

    else
        Select.fromGrouped
            { enumValues = List.sortBy groupAndLabel conditionsThatCanBeEnabled
            , valuePickedMessage = addCondition
            , optionLabel = getLabel
            , groupLabel = Conditions.getCategory
            , dummyLabel = "-- Přidat podmínku --"
            , enabled = True
            }


conditionSubForm : FilteredItem -> Condition -> Html Msg
conditionSubForm item =
    let
        wrap =
            closeableWrapper item
    in
    processCondition
        { amount = wrap Amount << Html.map AmountMsg << Amount.form
        , currentDaysPastDue = wrap Current_Days_Past_Due << Html.map CurrentDaysPastDueMsg << CurrentDaysPastDue.form
        , daysSinceLastPastDue = wrap Days_Since_Last_Past_Due << Html.map DaysSinceLastPastDueMsg << DaysSinceLastPastDue.form
        , elapsedTermMonths = wrap Elapsed_Term_Months << Html.map ElapsedTermMonthsMsg << ElapsedTermMonths.form
        , elapsedTermPercent = wrap Elapsed_Term_Percent << Html.map ElapsedTermPercentMsg << ElapsedTermPercent.form
        , health = wrap Health << Html.map HealthMsg << Health.form
        , income = wrap Income << Html.map IncomeMsg << Income.form
        , insurance = wrap Insurance << Html.map InsuranceConditionChanged << Insurance.form
        , interest = wrap Interest << Html.map InterestMsg << Interest.form
        , loanAnnuity = wrap Loan_Annuity << Html.map LoanAnnuityMsg << LoanAnnuity.form
        , longestDaysPastDue = wrap Longest_Days_Past_Due << Html.map LongestDaysPastDueMsg << LongestDaysPastDue.form
        , originalTermMonths = wrap Original_Term_Months << Html.map OriginalTermMonthsMsg << OriginalTermMonths.form
        , purpose = wrap Purpose << Html.map PurposeMsg << Purpose.form
        , region = wrap Region << Html.map RegionMsg << Region.form
        , relativeProfit = wrap Relative_Profit << Html.map RelativeProfitMsg << RelativeProfit.form
        , relativeSaleDiscount = wrap Relative_Sale_Discount << Html.map RelativeSaleDiscountMsg << RelativeSaleDiscount.form
        , remainingAmount = wrap Remaining_Amount << Html.map RemainingAmountMsg << RemainingAmount.form
        , remainingTermMonths = wrap Remaining_Term_Months << Html.map TermMonthsMsg << RemainingTermMonths.form
        , revenueRate = wrap Revenue_Rate << Html.map RevenueRateMsg << RevenueRate.form
        , saleFee = wrap Sale_Fee << Html.map SaleFeeConditionChanged << SaleFee.form
        , story = wrap Story << Html.map StoryConditionChanged << Story.form
        , termPercent = wrap Term_Percent << Html.map TermPercentMsg << TermPercent.form
        }


getVisibleLabel : FilteredItem -> ConditionType -> String
getVisibleLabel filteredItem conditionType =
    case conditionType of
        Amount ->
            amountConditionLabel filteredItem

        Current_Days_Past_Due ->
            "Aktuální doba po splatnosti (ve dnech)"

        Days_Since_Last_Past_Due ->
            "Doba od posledního dne po splatnosti (ve dnech)"

        Elapsed_Term_Months ->
            -- Note that this string contains &nbsp; entered by pasting escape sequence "\x00A0"
            -- (which is then transformed into corresponding unicode char by elm-format)
            "Uhrazeno splátek (v\u{00A0}měsících)"

        Elapsed_Term_Percent ->
            "Uhrazeno splátek (v\u{00A0}%)"

        Health ->
            "Historie splácení"

        Income ->
            "Zdroj příjmů klienta"

        Insurance ->
            "Pojištění"

        Interest ->
            "Úrok"

        Original_Term_Months ->
            "Původní délka půjčky"

        Loan_Annuity ->
            "Měsíční splátka"

        Longest_Days_Past_Due ->
            "Nejdelší doba po splatnosti (ve dnech)"

        Purpose ->
            "Účel půjčky"

        Region ->
            "Kraj klienta"

        Relative_Profit ->
            "Dosažený výnos"

        Relative_Sale_Discount ->
            "Sleva"

        Remaining_Amount ->
            "Zbývající jistina"

        Remaining_Term_Months ->
            termConditionLabel filteredItem "(v\u{00A0}měsících)"

        Revenue_Rate ->
            "Optimální výnos"

        Sale_Fee ->
            "Poplatek za prodej"

        Story ->
            "Příběh"

        Term_Percent ->
            termConditionLabel filteredItem "(v\u{00A0}%)"


termConditionLabel : FilteredItem -> String -> String
termConditionLabel filteredItem unitStr =
    case filteredItem of
        Loan ->
            "Délka půjčky " ++ unitStr

        _ ->
            "Zbývající délka půjčky " ++ unitStr


amountConditionLabel : FilteredItem -> String
amountConditionLabel filteredItem =
    case filteredItem of
        Loan ->
            "Výše půjčky"

        _ ->
            "Původní výše půjčky"


closeableWrapper : FilteredItem -> ConditionType -> Html Msg -> Html Msg
closeableWrapper filteredItem conditionType subform =
    let
        removeButton =
            Html.span [ onClick (RemoveCondition conditionType), class "float-right" ] [ Html.text "✖" ]

        conditionLabel =
            Html.text <| getVisibleLabel filteredItem conditionType
    in
    Grid.row [ Row.attrs [ class "condition-subform" ] ]
        [ Grid.col [ Col.xs3 ] [ conditionLabel ]
        , Grid.col [ Col.xs8 ] [ subform ]
        , Grid.col [ Col.xs1 ] [ removeButton ]
        ]



-- UPDATE


type
    Msg
    -- Forwarding messages to individual condition sub-forms
    = AmountMsg AmountMsg
    | CurrentDaysPastDueMsg CurrentDaysPastDueMsg
    | DaysSinceLastPastDueMsg DaysSinceLastPastDueMsg
    | ElapsedTermMonthsMsg ElapsedTermMonthsMsg
    | ElapsedTermPercentMsg ElapsedTermPercentMsg
    | HealthMsg HealthMsg
    | InsuranceConditionChanged InsuranceCondition
    | InterestMsg InterestMsg
    | IncomeMsg IncomeMsg
    | OriginalTermMonthsMsg OriginalTermMonthsMsg
    | LoanAnnuityMsg LoanAnnuityMsg
    | LongestDaysPastDueMsg LongestDaysPastDueMsg
    | PurposeMsg PurposeMsg
    | RegionMsg RegionMsg
    | RelativeProfitMsg RelativeProfitMsg
    | RelativeSaleDiscountMsg RelativeSaleDiscountMsg
    | RemainingAmountMsg RemainingAmountMsg
    | RevenueRateMsg RevenueRateMsg
    | SaleFeeConditionChanged SaleFeeCondition
    | StoryConditionChanged StoryCondition
    | TermMonthsMsg RemainingTermMonthsMsg
    | TermPercentMsg TermPercentMsg
      -- Control enabling / disabling conditions
    | AddCondition Condition
    | RemoveCondition ConditionType


update : Msg -> Model -> Model
update msg =
    case msg of
        AmountMsg m ->
            Conditions.updateAmount m

        CurrentDaysPastDueMsg m ->
            Conditions.updateCurrentDaysPastDueMsg m

        DaysSinceLastPastDueMsg m ->
            Conditions.updateDaysSinceLastPastDue m

        ElapsedTermMonthsMsg m ->
            Conditions.updateElapsedTermMonths m

        ElapsedTermPercentMsg m ->
            Conditions.updateElapsedTermPercent m

        HealthMsg m ->
            Conditions.updateHealth m

        IncomeMsg m ->
            Conditions.updateIncome m

        InsuranceConditionChanged c ->
            Conditions.setInsuranceCondition c

        InterestMsg m ->
            Conditions.updateInterest m

        OriginalTermMonthsMsg m ->
            Conditions.updateOriginalTermMonths m

        LoanAnnuityMsg m ->
            Conditions.updateLoanAnnuity m

        LongestDaysPastDueMsg m ->
            Conditions.updateLongestDaysPastDue m

        PurposeMsg m ->
            Conditions.updatePurpose m

        RegionMsg m ->
            Conditions.updateRegion m

        RelativeProfitMsg m ->
            Conditions.updateRelativeProfit m

        RelativeSaleDiscountMsg m ->
            Conditions.updateRelativeSaleDiscount m

        RemainingAmountMsg m ->
            Conditions.updateRemainingAmount m

        RevenueRateMsg m ->
            Conditions.updateRevenueRate m

        SaleFeeConditionChanged c ->
            Conditions.setSaleFeeCondition c

        StoryConditionChanged c ->
            Conditions.setStoryCondition c

        TermMonthsMsg m ->
            Conditions.updateTermMonths m

        TermPercentMsg m ->
            Conditions.updateTermPercent m

        AddCondition c ->
            Conditions.addCondition c

        RemoveCondition ct ->
            Conditions.removeCondition ct
