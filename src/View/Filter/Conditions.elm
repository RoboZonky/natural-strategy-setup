module View.Filter.Conditions exposing (Msg, conditionTypesThatApplyTo, form, getVisibleLabel, update)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Data.Filter exposing (FilteredItem(..))
import Data.Filter.Complexity exposing (FilterComplexity(..))
import Data.Filter.Conditions as C exposing (Condition(..), ConditionType(..), Conditions, processCondition)
import Data.Filter.Conditions.Amount as Amount exposing (Amount(..), AmountCondition(..), AmountMsg)
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonths(..), ElapsedTermMonthsCondition(..), ElapsedTermMonthsMsg)
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercent(..), ElapsedTermPercentCondition(..), ElapsedTermPercentMsg)
import Data.Filter.Conditions.Health as Health exposing (HealthMsg)
import Data.Filter.Conditions.Income as Income exposing (Income(..), IncomeCondition(..), IncomeMsg)
import Data.Filter.Conditions.Insurance as Insurance exposing (Insurance(..), InsuranceCondition(..))
import Data.Filter.Conditions.Interest as Interest exposing (Interest(..), InterestCondition(..), InterestMsg)
import Data.Filter.Conditions.LoanAnnuity as LoanAnnuity exposing (LoanAnnuity(..), LoanAnnuityCondition(..), LoanAnnuityMsg)
import Data.Filter.Conditions.OriginalTermMonths as OriginalTermMonths exposing (OriginalTermMonthsMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (Purpose(..), PurposeCondition(..), PurposeMsg)
import Data.Filter.Conditions.Region as Region exposing (Region(..), RegionCondition(..), RegionMsg)
import Data.Filter.Conditions.RelativeProfit as RelativeProfit exposing (RelativeProfitMsg)
import Data.Filter.Conditions.RelativeSaleDiscount as RelativeSaleDiscount exposing (RelativeSaleDiscountMsg)
import Data.Filter.Conditions.RemainingAmount as RemainingAmount exposing (RemainingAmount(..), RemainingAmountCondition(..), RemainingAmountMsg)
import Data.Filter.Conditions.RemainingTermMonths as RemainingTermMonths exposing (RemainingTermMonths(..), RemainingTermMonthsCondition(..), RemainingTermMonthsMsg)
import Data.Filter.Conditions.RevenueRate as RevenueRate exposing (RevenueRate(..), RevenueRateCondition(..), RevenueRateMsg)
import Data.Filter.Conditions.SaleFee as SaleFee exposing (SaleFee(..), SaleFeeCondition(..))
import Data.Filter.Conditions.Story as Story exposing (Story(..), StoryCondition(..))
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercent(..), TermPercentCondition(..), TermPercentMsg)
import Html exposing (Html)
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
            List.filter (\c -> List.member c validConditions) <| C.getDisabledConditionTypes conditions

        addCondition conditionType =
            AddCondition <| C.getDefaultCondition conditionType

        getLabel : ConditionType -> String
        getLabel =
            getVisibleLabel filteredItem
    in
    if List.isEmpty conditionsThatCanBeEnabled then
        Html.text ""

    else
        Select.from
            { enumValues = List.sortBy getLabel conditionsThatCanBeEnabled
            , valuePickedMessage = addCondition
            , showVisibleLabel = getLabel
            , defaultOption = DummyOption "-- Přidat podmínku --"
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
        , elapsedTermMonths = wrap Elapsed_Term_Months << Html.map ElapsedTermMonthsMsg << ElapsedTermMonths.form
        , elapsedTermPercent = wrap Elapsed_Term_Percent << Html.map ElapsedTermPercentMsg << ElapsedTermPercent.form
        , health = wrap Health << Html.map HealthMsg << Health.form
        , income = wrap Income << Html.map IncomeMsg << Income.form
        , insurance = wrap Insurance << Html.map InsuranceConditionChanged << Insurance.form
        , interest = wrap Interest << Html.map InterestMsg << Interest.form
        , loanAnnuity = wrap Loan_Annuity << Html.map LoanAnnuityMsg << LoanAnnuity.form
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
            "Původní délka úvěru"

        Loan_Annuity ->
            "Měsíční splátka"

        Purpose ->
            "Účel úvěru"

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
    | ElapsedTermMonthsMsg ElapsedTermMonthsMsg
    | ElapsedTermPercentMsg ElapsedTermPercentMsg
    | HealthMsg HealthMsg
    | InsuranceConditionChanged InsuranceCondition
    | InterestMsg InterestMsg
    | IncomeMsg IncomeMsg
    | OriginalTermMonthsMsg OriginalTermMonthsMsg
    | LoanAnnuityMsg LoanAnnuityMsg
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
            C.updateAmount m

        ElapsedTermMonthsMsg m ->
            C.updateElapsedTermMonths m

        ElapsedTermPercentMsg m ->
            C.updateElapsedTermPercent m

        HealthMsg m ->
            C.updateHealth m

        IncomeMsg m ->
            C.updateIncome m

        InsuranceConditionChanged c ->
            C.setInsuranceCondition c

        InterestMsg m ->
            C.updateInterest m

        OriginalTermMonthsMsg m ->
            C.updateOriginalTermMonths m

        LoanAnnuityMsg m ->
            C.updateLoanAnnuity m

        PurposeMsg m ->
            C.updatePurpose m

        RegionMsg m ->
            C.updateRegion m

        RelativeProfitMsg m ->
            C.updateRelativeProfit m

        RelativeSaleDiscountMsg m ->
            C.updateRelativeSaleDiscount m

        RemainingAmountMsg m ->
            C.updateRemainingAmount m

        RevenueRateMsg m ->
            C.updateRevenueRate m

        SaleFeeConditionChanged c ->
            C.setSaleFeeCondition c

        StoryConditionChanged c ->
            C.setStoryCondition c

        TermMonthsMsg m ->
            C.updateTermMonths m

        TermPercentMsg m ->
            C.updateTermPercent m

        AddCondition c ->
            C.addCondition c

        RemoveCondition ct ->
            C.removeCondition ct
