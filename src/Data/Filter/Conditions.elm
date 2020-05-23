module Data.Filter.Conditions exposing
    ( Condition(..)
    , ConditionCont
    , ConditionType(..)
    , Conditions
    , addCondition
    , conditionsDecoder
    , conditionsValidationErrors
    , emptyConditions
    , encodeConditions
    , getCategory
    , getDefaultCondition
    , getDisabledConditionTypes
    , getEnabledConditionTypes
    , getEnabledConditions
    , processCondition
    , removeCondition
    , removeConditions
    , renderCondition
    , setInsuranceCondition
    , setSaleFeeCondition
    , setStoryCondition
    , updateAmount
    , updateCurrentDaysPastDueMsg
    , updateDaysSinceLastPastDue
    , updateElapsedTermMonths
    , updateElapsedTermPercent
    , updateHealth
    , updateIncome
    , updateInterest
    , updateLoanAnnuity
    , updateLongestDaysPastDue
    , updateOriginalTermMonths
    , updatePurpose
    , updateRegion
    , updateRelativeProfit
    , updateRelativeSaleDiscount
    , updateRemainingAmount
    , updateRevenueRate
    , updateTermMonths
    , updateTermPercent
    )

import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition, AmountMsg)
import Data.Filter.Conditions.CurrentDaysPastDue as CurrentDaysPastDue exposing (CurrentDaysPastDueCondition, CurrentDaysPastDueMsg)
import Data.Filter.Conditions.DaysSinceLastPastDue as DaysSinceLastPastDue exposing (DaysSinceLastPastDueCondition, DaysSinceLastPastDueMsg)
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonthsCondition, ElapsedTermMonthsMsg)
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercentCondition, ElapsedTermPercentMsg)
import Data.Filter.Conditions.Health as Health exposing (HealthCondition, HealthMsg)
import Data.Filter.Conditions.Income as Income exposing (IncomeCondition, IncomeMsg)
import Data.Filter.Conditions.Insurance as Insurance exposing (InsuranceCondition)
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition, InterestMsg)
import Data.Filter.Conditions.LoanAnnuity as LoanAnnuity exposing (LoanAnnuityCondition, LoanAnnuityMsg)
import Data.Filter.Conditions.LongestDaysPastDue as LongestDaysPastDue exposing (LongestDaysPastDueCondition, LongestDaysPastDueMsg)
import Data.Filter.Conditions.OriginalTermMonths as OriginalTermMonths exposing (OriginalTermMonthsCondition, OriginalTermMonthsMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (PurposeCondition, PurposeMsg)
import Data.Filter.Conditions.Region as Region exposing (RegionCondition, RegionMsg)
import Data.Filter.Conditions.RelativeProfit as RelativeProfit exposing (RelativeProfitCondition, RelativeProfitMsg)
import Data.Filter.Conditions.RelativeSaleDiscount as RelativeSaleDiscount exposing (RelativeSaleDiscountCondition, RelativeSaleDiscountMsg)
import Data.Filter.Conditions.RemainingAmount as RemainingAmount exposing (RemainingAmountCondition, RemainingAmountMsg)
import Data.Filter.Conditions.RemainingTermMonths as RemainingTermMonths exposing (RemainingTermMonthsCondition, RemainingTermMonthsMsg)
import Data.Filter.Conditions.RevenueRate as RevenueRate exposing (RevenueRateCondition, RevenueRateMsg)
import Data.Filter.Conditions.SaleFee as SaleFee exposing (SaleFeeCondition)
import Data.Filter.Conditions.Story as Story exposing (StoryCondition)
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercentCondition, TermPercentMsg)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing (andMap, optionalField)
import Json.Encode as Encode exposing (Value)
import List


type alias Conditions =
    { region : Maybe RegionCondition
    , income : Maybe IncomeCondition
    , purpose : Maybe PurposeCondition
    , story : Maybe StoryCondition
    , remainingTermMonths : Maybe RemainingTermMonthsCondition
    , termPercent : Maybe TermPercentCondition
    , elapsedTermMonths : Maybe ElapsedTermMonthsCondition
    , elapsedTermPercent : Maybe ElapsedTermPercentCondition
    , interest : Maybe InterestCondition
    , amount : Maybe AmountCondition
    , insurance : Maybe InsuranceCondition
    , remainingAmount : Maybe RemainingAmountCondition
    , loanAnnuity : Maybe LoanAnnuityCondition
    , revenueRate : Maybe RevenueRateCondition
    , saleFee : Maybe SaleFeeCondition
    , relativeProfit : Maybe RelativeProfitCondition
    , health : Maybe HealthCondition
    , originalTermMonths : Maybe OriginalTermMonthsCondition
    , relativeSaleDiscount : Maybe RelativeSaleDiscountCondition
    , currentDaysPastDue : Maybe CurrentDaysPastDueCondition
    , daysSinceLastPastDue : Maybe DaysSinceLastPastDueCondition
    , longestDaysPastDue : Maybe LongestDaysPastDueCondition
    }


emptyConditions : Conditions
emptyConditions =
    { region = Nothing
    , income = Nothing
    , purpose = Nothing
    , story = Nothing
    , remainingTermMonths = Nothing
    , termPercent = Nothing
    , elapsedTermMonths = Nothing
    , elapsedTermPercent = Nothing
    , interest = Nothing
    , amount = Nothing
    , insurance = Nothing
    , remainingAmount = Nothing
    , loanAnnuity = Nothing
    , revenueRate = Nothing
    , saleFee = Nothing
    , relativeProfit = Nothing
    , health = Nothing
    , originalTermMonths = Nothing
    , relativeSaleDiscount = Nothing
    , currentDaysPastDue = Nothing
    , longestDaysPastDue = Nothing
    , daysSinceLastPastDue = Nothing
    }


type Condition
    = Condition_Amount AmountCondition
    | Condition_Current_Days_Past_Due CurrentDaysPastDueCondition
    | Condition_Days_Since_Last_Past_Due DaysSinceLastPastDueCondition
    | Condition_Elapsed_Term_Months ElapsedTermMonthsCondition
    | Condition_Elapsed_Term_Percent ElapsedTermPercentCondition
    | Condition_Health HealthCondition
    | Condition_Income IncomeCondition
    | Condition_Insurance InsuranceCondition
    | Condition_Interest InterestCondition
    | Condition_Loan_Annuity LoanAnnuityCondition
    | Condition_Longest_Days_Past_Due LongestDaysPastDueCondition
    | Condition_Original_Term_Months OriginalTermMonthsCondition
    | Condition_Purpose PurposeCondition
    | Condition_Relative_Profit RelativeProfitCondition
    | Condition_Relative_Sale_Discount RelativeSaleDiscountCondition
    | Condition_Remaining_Amount RemainingAmountCondition
    | Condition_Remaining_Term_Months RemainingTermMonthsCondition
    | Condition_Region RegionCondition
    | Condition_Revenue_Rate RevenueRateCondition
    | Condition_Story StoryCondition
    | Condition_Sale_Fee SaleFeeCondition
    | Condition_Term_Percent TermPercentCondition


type ConditionType
    = Amount
    | Current_Days_Past_Due
    | Days_Since_Last_Past_Due
    | Elapsed_Term_Months
    | Elapsed_Term_Percent
    | Health
    | Income
    | Insurance
    | Interest
    | Loan_Annuity
    | Longest_Days_Past_Due
    | Original_Term_Months
    | Purpose
    | Region
    | Relative_Profit
    | Relative_Sale_Discount
    | Remaining_Amount
    | Remaining_Term_Months
    | Revenue_Rate
    | Sale_Fee
    | Story
    | Term_Percent


type alias ConditionCont a =
    { amount : AmountCondition -> a
    , currentDaysPastDue : CurrentDaysPastDueCondition -> a
    , daysSinceLastPastDue : DaysSinceLastPastDueCondition -> a
    , elapsedTermMonths : ElapsedTermMonthsCondition -> a
    , elapsedTermPercent : ElapsedTermPercentCondition -> a
    , health : HealthCondition -> a
    , income : IncomeCondition -> a
    , insurance : InsuranceCondition -> a
    , interest : InterestCondition -> a
    , loanAnnuity : LoanAnnuityCondition -> a
    , longestDaysPastDue : LongestDaysPastDueCondition -> a
    , originalTermMonths : OriginalTermMonthsCondition -> a
    , purpose : PurposeCondition -> a
    , region : RegionCondition -> a
    , relativeProfit : RelativeProfitCondition -> a
    , relativeSaleDiscount : RelativeSaleDiscountCondition -> a
    , remainingAmount : RemainingAmountCondition -> a
    , remainingTermMonths : RemainingTermMonthsCondition -> a
    , revenueRate : RevenueRateCondition -> a
    , saleFee : SaleFeeCondition -> a
    , story : StoryCondition -> a
    , termPercent : TermPercentCondition -> a
    }


processCondition : ConditionCont a -> Condition -> a
processCondition cont c =
    case c of
        Condition_Amount x ->
            cont.amount x

        Condition_Current_Days_Past_Due x ->
            cont.currentDaysPastDue x

        Condition_Days_Since_Last_Past_Due x ->
            cont.daysSinceLastPastDue x

        Condition_Elapsed_Term_Months x ->
            cont.elapsedTermMonths x

        Condition_Elapsed_Term_Percent x ->
            cont.elapsedTermPercent x

        Condition_Health x ->
            cont.health x

        Condition_Income x ->
            cont.income x

        Condition_Insurance x ->
            cont.insurance x

        Condition_Interest x ->
            cont.interest x

        Condition_Loan_Annuity x ->
            cont.loanAnnuity x

        Condition_Longest_Days_Past_Due x ->
            cont.longestDaysPastDue x

        Condition_Original_Term_Months x ->
            cont.originalTermMonths x

        Condition_Purpose x ->
            cont.purpose x

        Condition_Relative_Profit x ->
            cont.relativeProfit x

        Condition_Relative_Sale_Discount x ->
            cont.relativeSaleDiscount x

        Condition_Remaining_Amount x ->
            cont.remainingAmount x

        Condition_Remaining_Term_Months x ->
            cont.remainingTermMonths x

        Condition_Region x ->
            cont.region x

        Condition_Revenue_Rate x ->
            cont.revenueRate x

        Condition_Story x ->
            cont.story x

        Condition_Sale_Fee x ->
            cont.saleFee x

        Condition_Term_Percent x ->
            cont.termPercent x


getType : Condition -> ConditionType
getType =
    processCondition
        { amount = always Amount
        , currentDaysPastDue = always Current_Days_Past_Due
        , daysSinceLastPastDue = always Days_Since_Last_Past_Due
        , elapsedTermMonths = always Elapsed_Term_Months
        , elapsedTermPercent = always Elapsed_Term_Percent
        , health = always Health
        , income = always Income
        , insurance = always Insurance
        , interest = always Interest
        , loanAnnuity = always Loan_Annuity
        , longestDaysPastDue = always Longest_Days_Past_Due
        , originalTermMonths = always Original_Term_Months
        , purpose = always Purpose
        , region = always Region
        , relativeProfit = always Relative_Profit
        , relativeSaleDiscount = always Relative_Sale_Discount
        , remainingAmount = always Remaining_Amount
        , remainingTermMonths = always Remaining_Term_Months
        , revenueRate = always Revenue_Rate
        , saleFee = always Sale_Fee
        , story = always Story
        , termPercent = always Term_Percent
        }


renderCondition : Condition -> String
renderCondition =
    processCondition
        { amount = Amount.renderCondition
        , currentDaysPastDue = CurrentDaysPastDue.renderCondition
        , daysSinceLastPastDue = DaysSinceLastPastDue.renderCondition
        , elapsedTermMonths = ElapsedTermMonths.renderCondition
        , elapsedTermPercent = ElapsedTermPercent.renderCondition
        , health = Health.renderCondition
        , income = Income.renderCondition
        , insurance = Insurance.renderCondition
        , interest = Interest.renderCondition
        , loanAnnuity = LoanAnnuity.renderCondition
        , longestDaysPastDue = LongestDaysPastDue.renderCondition
        , originalTermMonths = OriginalTermMonths.renderCondition
        , purpose = Purpose.renderCondition
        , region = Region.renderCondition
        , relativeProfit = RelativeProfit.renderCondition
        , relativeSaleDiscount = RelativeSaleDiscount.renderCondition
        , remainingAmount = RemainingAmount.renderCondition
        , remainingTermMonths = RemainingTermMonths.renderCondition
        , revenueRate = RevenueRate.renderCondition
        , saleFee = SaleFee.renderCondition
        , story = Story.renderCondition
        , termPercent = TermPercent.renderCondition
        }


conditionValidationError : Condition -> List String
conditionValidationError =
    processCondition
        { amount = Amount.validationErrors
        , currentDaysPastDue = CurrentDaysPastDue.validationErrors
        , daysSinceLastPastDue = DaysSinceLastPastDue.validationErrors
        , elapsedTermMonths = ElapsedTermMonths.validationErrors
        , elapsedTermPercent = ElapsedTermPercent.validationErrors
        , health = Health.validationErrors
        , income = Income.validationErrors
        , insurance = always [{- Insurance condition can't be invalid -> valid. errors list always empty -}]
        , interest = Interest.validationErrors
        , originalTermMonths = OriginalTermMonths.validationErrors
        , loanAnnuity = LoanAnnuity.validationErrors
        , longestDaysPastDue = LongestDaysPastDue.validationErrors
        , purpose = Purpose.validationErrors
        , region = Region.validationErrors
        , remainingAmount = RemainingAmount.validationErrors
        , remainingTermMonths = RemainingTermMonths.validationErrors
        , relativeProfit = RelativeProfit.validationErrors
        , relativeSaleDiscount = RelativeSaleDiscount.validationErrors
        , revenueRate = RevenueRate.validationErrors
        , saleFee = always [{- Sale fee condition can't be invalid -> valid. errors list always empty -}]
        , story = always [{- Story condition can't be invalid -> valid. errors list always empty -}]
        , termPercent = TermPercent.validationErrors
        }


addCondition : Condition -> Conditions -> Conditions
addCondition =
    processCondition
        { amount = \c cs -> { cs | amount = Just c }
        , currentDaysPastDue = \c cs -> { cs | currentDaysPastDue = Just c }
        , daysSinceLastPastDue = \c cs -> { cs | daysSinceLastPastDue = Just c }
        , elapsedTermMonths = \c cs -> { cs | elapsedTermMonths = Just c }
        , elapsedTermPercent = \c cs -> { cs | elapsedTermPercent = Just c }
        , health = \c cs -> { cs | health = Just c }
        , income = \c cs -> { cs | income = Just c }
        , insurance = \c cs -> { cs | insurance = Just c }
        , interest = \c cs -> { cs | interest = Just c }
        , loanAnnuity = \c cs -> { cs | loanAnnuity = Just c }
        , longestDaysPastDue = \c cs -> { cs | longestDaysPastDue = Just c }
        , originalTermMonths = \c cs -> { cs | originalTermMonths = Just c }
        , purpose = \c cs -> { cs | purpose = Just c }
        , region = \c cs -> { cs | region = Just c }
        , relativeProfit = \c cs -> { cs | relativeProfit = Just c }
        , relativeSaleDiscount = \c cs -> { cs | relativeSaleDiscount = Just c }
        , remainingAmount = \c cs -> { cs | remainingAmount = Just c }
        , remainingTermMonths = \c cs -> { cs | remainingTermMonths = Just c }
        , revenueRate = \c cs -> { cs | revenueRate = Just c }
        , saleFee = \c cs -> { cs | saleFee = Just c }
        , story = \c cs -> { cs | story = Just c }
        , termPercent = \c cs -> { cs | termPercent = Just c }
        }


conditionsValidationErrors : String -> Conditions -> List String
conditionsValidationErrors errorPrefix =
    List.map (\e -> errorPrefix ++ e) << List.concatMap conditionValidationError << getEnabledConditions


getDisabledConditionTypes : Conditions -> List ConditionType
getDisabledConditionTypes cs =
    let
        addIfNothing : (Conditions -> Maybe a) -> ConditionType -> List ConditionType
        addIfNothing field conditionType =
            Maybe.withDefault [ conditionType ] <| Maybe.map (always []) <| field cs
    in
    List.concat
        [ addIfNothing .amount Amount
        , addIfNothing .currentDaysPastDue Current_Days_Past_Due
        , addIfNothing .daysSinceLastPastDue Days_Since_Last_Past_Due
        , addIfNothing .elapsedTermMonths Elapsed_Term_Months
        , addIfNothing .elapsedTermPercent Elapsed_Term_Percent
        , addIfNothing .health Health
        , addIfNothing .income Income
        , addIfNothing .insurance Insurance
        , addIfNothing .interest Interest
        , addIfNothing .originalTermMonths Original_Term_Months
        , addIfNothing .loanAnnuity Loan_Annuity
        , addIfNothing .longestDaysPastDue Longest_Days_Past_Due
        , addIfNothing .purpose Purpose
        , addIfNothing .region Region
        , addIfNothing .relativeProfit Relative_Profit
        , addIfNothing .relativeSaleDiscount Relative_Sale_Discount
        , addIfNothing .remainingAmount Remaining_Amount
        , addIfNothing .remainingTermMonths Remaining_Term_Months
        , addIfNothing .revenueRate Revenue_Rate
        , addIfNothing .saleFee Sale_Fee
        , addIfNothing .story Story
        , addIfNothing .termPercent Term_Percent
        ]


getEnabledConditions : Conditions -> List Condition
getEnabledConditions cs =
    List.filterMap identity
        [ Maybe.map Condition_Amount cs.amount
        , Maybe.map Condition_Current_Days_Past_Due cs.currentDaysPastDue
        , Maybe.map Condition_Days_Since_Last_Past_Due cs.daysSinceLastPastDue
        , Maybe.map Condition_Elapsed_Term_Months cs.elapsedTermMonths
        , Maybe.map Condition_Elapsed_Term_Percent cs.elapsedTermPercent
        , Maybe.map Condition_Health cs.health
        , Maybe.map Condition_Income cs.income
        , Maybe.map Condition_Insurance cs.insurance
        , Maybe.map Condition_Interest cs.interest
        , Maybe.map Condition_Loan_Annuity cs.loanAnnuity
        , Maybe.map Condition_Longest_Days_Past_Due cs.longestDaysPastDue
        , Maybe.map Condition_Original_Term_Months cs.originalTermMonths
        , Maybe.map Condition_Purpose cs.purpose
        , Maybe.map Condition_Region cs.region
        , Maybe.map Condition_Relative_Profit cs.relativeProfit
        , Maybe.map Condition_Relative_Sale_Discount cs.relativeSaleDiscount
        , Maybe.map Condition_Remaining_Amount cs.remainingAmount
        , Maybe.map Condition_Remaining_Term_Months cs.remainingTermMonths
        , Maybe.map Condition_Revenue_Rate cs.revenueRate
        , Maybe.map Condition_Sale_Fee cs.saleFee
        , Maybe.map Condition_Story cs.story
        , Maybe.map Condition_Term_Percent cs.termPercent
        ]


getEnabledConditionTypes : Conditions -> List ConditionType
getEnabledConditionTypes =
    List.map getType << getEnabledConditions


updateInterest : InterestMsg -> Conditions -> Conditions
updateInterest msg conditions =
    { conditions | interest = Maybe.map (Interest.update msg) conditions.interest }


updateAmount : AmountMsg -> Conditions -> Conditions
updateAmount msg conditions =
    { conditions | amount = Maybe.map (Amount.update msg) conditions.amount }


updateCurrentDaysPastDueMsg : CurrentDaysPastDueMsg -> Conditions -> Conditions
updateCurrentDaysPastDueMsg msg conditions =
    { conditions | currentDaysPastDue = Maybe.map (CurrentDaysPastDue.update msg) conditions.currentDaysPastDue }


updateDaysSinceLastPastDue : DaysSinceLastPastDueMsg -> Conditions -> Conditions
updateDaysSinceLastPastDue msg conditions =
    { conditions | daysSinceLastPastDue = Maybe.map (DaysSinceLastPastDue.update msg) conditions.daysSinceLastPastDue }


updateRemainingAmount : RemainingAmountMsg -> Conditions -> Conditions
updateRemainingAmount msg conditions =
    { conditions | remainingAmount = Maybe.map (RemainingAmount.update msg) conditions.remainingAmount }


updateRevenueRate : RevenueRateMsg -> Conditions -> Conditions
updateRevenueRate msg conditions =
    { conditions | revenueRate = Maybe.map (RevenueRate.update msg) conditions.revenueRate }


updatePurpose : PurposeMsg -> Conditions -> Conditions
updatePurpose msg conditions =
    { conditions | purpose = Maybe.map (Purpose.update msg) conditions.purpose }


updateTermMonths : RemainingTermMonthsMsg -> Conditions -> Conditions
updateTermMonths msg conditions =
    { conditions | remainingTermMonths = Maybe.map (RemainingTermMonths.update msg) conditions.remainingTermMonths }


updateTermPercent : TermPercentMsg -> Conditions -> Conditions
updateTermPercent msg conditions =
    { conditions | termPercent = Maybe.map (TermPercent.update msg) conditions.termPercent }


updateElapsedTermMonths : ElapsedTermMonthsMsg -> Conditions -> Conditions
updateElapsedTermMonths msg conditions =
    { conditions | elapsedTermMonths = Maybe.map (ElapsedTermMonths.update msg) conditions.elapsedTermMonths }


updateElapsedTermPercent : ElapsedTermPercentMsg -> Conditions -> Conditions
updateElapsedTermPercent msg conditions =
    { conditions | elapsedTermPercent = Maybe.map (ElapsedTermPercent.update msg) conditions.elapsedTermPercent }


updateHealth : HealthMsg -> Conditions -> Conditions
updateHealth msg conditions =
    { conditions | health = Maybe.map (Health.update msg) conditions.health }


updateIncome : IncomeMsg -> Conditions -> Conditions
updateIncome msg conditions =
    { conditions | income = Maybe.map (Income.update msg) conditions.income }


updateRegion : RegionMsg -> Conditions -> Conditions
updateRegion msg conditions =
    { conditions | region = Maybe.map (Region.update msg) conditions.region }


updateRelativeProfit : RelativeProfitMsg -> Conditions -> Conditions
updateRelativeProfit msg conditions =
    { conditions | relativeProfit = Maybe.map (RelativeProfit.update msg) conditions.relativeProfit }


updateLoanAnnuity : LoanAnnuityMsg -> Conditions -> Conditions
updateLoanAnnuity msg conditions =
    { conditions | loanAnnuity = Maybe.map (LoanAnnuity.update msg) conditions.loanAnnuity }


updateLongestDaysPastDue : LongestDaysPastDueMsg -> Conditions -> Conditions
updateLongestDaysPastDue msg conditions =
    { conditions | longestDaysPastDue = Maybe.map (LongestDaysPastDue.update msg) conditions.longestDaysPastDue }


updateOriginalTermMonths : OriginalTermMonthsMsg -> Conditions -> Conditions
updateOriginalTermMonths msg conditions =
    { conditions | originalTermMonths = Maybe.map (OriginalTermMonths.update msg) conditions.originalTermMonths }


updateRelativeSaleDiscount : RelativeSaleDiscountMsg -> Conditions -> Conditions
updateRelativeSaleDiscount msg conditions =
    { conditions | relativeSaleDiscount = Maybe.map (RelativeSaleDiscount.update msg) conditions.relativeSaleDiscount }


setInsuranceCondition : InsuranceCondition -> Conditions -> Conditions
setInsuranceCondition insuranceCondition conditions =
    { conditions | insurance = Just insuranceCondition }


setSaleFeeCondition : SaleFeeCondition -> Conditions -> Conditions
setSaleFeeCondition saleFeeCondition conditions =
    { conditions | saleFee = Just saleFeeCondition }


setStoryCondition : StoryCondition -> Conditions -> Conditions
setStoryCondition storyCondition conditions =
    { conditions | story = Just storyCondition }


removeCondition : ConditionType -> Conditions -> Conditions
removeCondition conditionType cs =
    case conditionType of
        Amount ->
            { cs | amount = Nothing }

        Current_Days_Past_Due ->
            { cs | currentDaysPastDue = Nothing }

        Days_Since_Last_Past_Due ->
            { cs | daysSinceLastPastDue = Nothing }

        Elapsed_Term_Months ->
            { cs | elapsedTermMonths = Nothing }

        Elapsed_Term_Percent ->
            { cs | elapsedTermPercent = Nothing }

        Health ->
            { cs | health = Nothing }

        Income ->
            { cs | income = Nothing }

        Insurance ->
            { cs | insurance = Nothing }

        Interest ->
            { cs | interest = Nothing }

        Longest_Days_Past_Due ->
            { cs | longestDaysPastDue = Nothing }

        Original_Term_Months ->
            { cs | originalTermMonths = Nothing }

        Loan_Annuity ->
            { cs | loanAnnuity = Nothing }

        Purpose ->
            { cs | purpose = Nothing }

        Region ->
            { cs | region = Nothing }

        Relative_Profit ->
            { cs | relativeProfit = Nothing }

        Relative_Sale_Discount ->
            { cs | relativeSaleDiscount = Nothing }

        Remaining_Amount ->
            { cs | remainingAmount = Nothing }

        Remaining_Term_Months ->
            { cs | remainingTermMonths = Nothing }

        Revenue_Rate ->
            { cs | revenueRate = Nothing }

        Sale_Fee ->
            { cs | saleFee = Nothing }

        Story ->
            { cs | story = Nothing }

        Term_Percent ->
            { cs | termPercent = Nothing }


removeConditions : List ConditionType -> Conditions -> Conditions
removeConditions conditionsToBeRemoved cs =
    List.foldl removeCondition cs conditionsToBeRemoved


getDefaultCondition : ConditionType -> Condition
getDefaultCondition conditionType =
    case conditionType of
        Amount ->
            Condition_Amount Amount.defaultCondition

        Current_Days_Past_Due ->
            Condition_Current_Days_Past_Due CurrentDaysPastDue.defaultCondition

        Days_Since_Last_Past_Due ->
            Condition_Days_Since_Last_Past_Due DaysSinceLastPastDue.defaultCondition

        Elapsed_Term_Months ->
            Condition_Elapsed_Term_Months ElapsedTermMonths.defaultCondition

        Elapsed_Term_Percent ->
            Condition_Elapsed_Term_Percent ElapsedTermPercent.defaultCondition

        Health ->
            Condition_Health Health.defaultCondition

        Income ->
            Condition_Income Income.defaultCondition

        Insurance ->
            Condition_Insurance Insurance.defaultCondition

        Interest ->
            Condition_Interest Interest.defaultCondition

        Original_Term_Months ->
            Condition_Original_Term_Months OriginalTermMonths.defaultCondition

        Loan_Annuity ->
            Condition_Loan_Annuity LoanAnnuity.defaultCondition

        Longest_Days_Past_Due ->
            Condition_Longest_Days_Past_Due LongestDaysPastDue.defaultCondition

        Purpose ->
            Condition_Purpose Purpose.defaultCondition

        Region ->
            Condition_Region Region.defaultCondition

        Relative_Profit ->
            Condition_Relative_Profit RelativeProfit.defaultCondition

        Relative_Sale_Discount ->
            Condition_Relative_Sale_Discount RelativeSaleDiscount.defaultCondition

        Remaining_Amount ->
            Condition_Remaining_Amount RemainingAmount.defaultCondition

        Remaining_Term_Months ->
            Condition_Remaining_Term_Months RemainingTermMonths.defaultCondition

        Revenue_Rate ->
            Condition_Revenue_Rate RevenueRate.defaultCondition

        Sale_Fee ->
            Condition_Sale_Fee SaleFee.defaultCondition

        Story ->
            Condition_Story Story.defaultCondition

        Term_Percent ->
            Condition_Term_Percent TermPercent.defaultCondition


getCategory : ConditionType -> String
getCategory conditionType =
    case conditionType of
        Amount ->
            "Půjčka"

        Current_Days_Past_Due ->
            "Splácení"

        Days_Since_Last_Past_Due ->
            "Splácení"

        Elapsed_Term_Months ->
            "Splácení"

        Elapsed_Term_Percent ->
            "Splácení"

        Health ->
            "Splácení"

        Income ->
            "Klient"

        Insurance ->
            "Půjčka"

        Interest ->
            "Půjčka"

        Original_Term_Months ->
            "Půjčka"

        Loan_Annuity ->
            "Půjčka"

        Longest_Days_Past_Due ->
            "Splácení"

        Purpose ->
            "Půjčka"

        Region ->
            "Klient"

        Relative_Profit ->
            "Výnos"

        Relative_Sale_Discount ->
            "Výnos"

        Remaining_Amount ->
            "Splácení"

        Remaining_Term_Months ->
            "Splácení"

        Revenue_Rate ->
            "Výnos"

        Sale_Fee ->
            "Výnos"

        Story ->
            "Půjčka"

        Term_Percent ->
            "Splácení"



-- JSON


encodeConditions : Conditions -> Value
encodeConditions conditions =
    getEnabledConditions conditions
        |> List.map encodeCondition
        |> Encode.object


encodeCondition : Condition -> ( String, Value )
encodeCondition =
    processCondition
        { amount = Tuple.pair "K" << Amount.encodeCondition
        , currentDaysPastDue = Tuple.pair "U" << CurrentDaysPastDue.encodeCondition
        , daysSinceLastPastDue = Tuple.pair "V" << DaysSinceLastPastDue.encodeCondition
        , elapsedTermMonths = Tuple.pair "H" << ElapsedTermMonths.encodeCondition
        , elapsedTermPercent = Tuple.pair "I" << ElapsedTermPercent.encodeCondition
        , health = Tuple.pair "R" << Health.encodeCondition
        , income = Tuple.pair "C" << Income.encodeCondition
        , insurance = Tuple.pair "L" << Insurance.encodeCondition
        , interest = Tuple.pair "J" << Interest.encodeCondition
        , loanAnnuity = Tuple.pair "N" << LoanAnnuity.encodeCondition
        , longestDaysPastDue = Tuple.pair "W" << LongestDaysPastDue.encodeCondition
        , originalTermMonths = Tuple.pair "S" << OriginalTermMonths.encodeCondition
        , purpose = Tuple.pair "D" << Purpose.encodeCondition
        , region = Tuple.pair "A" << Region.encodeCondition
        , relativeProfit = Tuple.pair "Q" << RelativeProfit.encodeCondition
        , relativeSaleDiscount = Tuple.pair "T" << RelativeSaleDiscount.encodeCondition
        , remainingAmount = Tuple.pair "M" << RemainingAmount.encodeCondition
        , remainingTermMonths = Tuple.pair "F" << RemainingTermMonths.encodeCondition
        , revenueRate = Tuple.pair "O" << RevenueRate.encodeCondition
        , saleFee = Tuple.pair "P" << SaleFee.encodeCondition
        , story = Tuple.pair "E" << Story.encodeCondition
        , termPercent = Tuple.pair "G" << TermPercent.encodeCondition
        }


conditionsDecoder : Decoder Conditions
conditionsDecoder =
    Decode.succeed Conditions
        |> andMap (optionalField "A" Region.conditionDecoder)
        -- "B" used to be rating condition, which was removed in strategy v2
        |> andMap (optionalField "C" Income.conditionDecoder)
        |> andMap (optionalField "D" Purpose.conditionDecoder)
        |> andMap (optionalField "E" Story.conditionDecoder)
        |> andMap (optionalField "F" RemainingTermMonths.conditionDecoder)
        |> andMap (optionalField "G" TermPercent.conditionDecoder)
        |> andMap (optionalField "H" ElapsedTermMonths.conditionDecoder)
        |> andMap (optionalField "I" ElapsedTermPercent.conditionDecoder)
        |> andMap (optionalField "J" Interest.conditionDecoder)
        |> andMap (optionalField "K" Amount.conditionDecoder)
        |> andMap (optionalField "L" Insurance.conditionDecoder)
        |> andMap (optionalField "M" RemainingAmount.conditionDecoder)
        |> andMap (optionalField "N" LoanAnnuity.conditionDecoder)
        |> andMap (optionalField "O" RevenueRate.conditionDecoder)
        |> andMap (optionalField "P" SaleFee.conditionDecoder)
        |> andMap (optionalField "Q" RelativeProfit.conditionDecoder)
        |> andMap (optionalField "R" Health.conditionDecoder)
        |> andMap (optionalField "S" OriginalTermMonths.conditionDecoder)
        |> andMap (optionalField "T" RelativeSaleDiscount.conditionDecoder)
        |> andMap (optionalField "U" CurrentDaysPastDue.conditionDecoder)
        |> andMap (optionalField "V" DaysSinceLastPastDue.conditionDecoder)
        |> andMap (optionalField "W" LongestDaysPastDue.conditionDecoder)
