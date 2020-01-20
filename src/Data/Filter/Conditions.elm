module Data.Filter.Conditions exposing
    ( Condition(..)
    , ConditionType(..)
    , Conditions
    , addCondition
    , conditionsDecoder
    , conditionsValidationErrors
    , emptyConditions
    , encodeConditions
    , getDefaultCondition
    , getDisabledConditionTypes
    , getEnabledConditionTypes
    , getEnabledConditions
    , removeCondition
    , removeConditions
    , renderCondition
    , updateAmount
    , updateElapsedTermMonths
    , updateElapsedTermPercent
    , updateHealth
    , updateIncome
    , updateInsurance
    , updateInterest
    , updateLoanAnnuity
    , updatePurpose
    , updateRegion
    , updateRelativeProfit
    , updateRemainingAmount
    , updateRevenueRate
    , updateSaleFee
    , updateStory
    , updateTermMonths
    , updateTermPercent
    )

import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition, AmountMsg)
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonthsCondition, ElapsedTermMonthsMsg)
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercentCondition, ElapsedTermPercentMsg)
import Data.Filter.Conditions.Health as Health exposing (HealthCondition, HealthMsg)
import Data.Filter.Conditions.Income as Income exposing (IncomeCondition, IncomeMsg)
import Data.Filter.Conditions.Insurance as Insurance exposing (InsuranceCondition, InsuranceMsg)
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition, InterestMsg)
import Data.Filter.Conditions.LoanAnnuity as LoanAnnuity exposing (LoanAnnuityCondition, LoanAnnuityMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (PurposeCondition, PurposeMsg)
import Data.Filter.Conditions.Region as Region exposing (RegionCondition, RegionMsg)
import Data.Filter.Conditions.RelativeProfit as RelativeProfit exposing (RelativeProfitCondition, RelativeProfitMsg)
import Data.Filter.Conditions.RemainingAmount as RemainingAmount exposing (RemainingAmountCondition, RemainingAmountMsg)
import Data.Filter.Conditions.RemainingTermMonths as RemainingTermMonths exposing (RemainingTermMonthsCondition, RemainingTermMonthsMsg)
import Data.Filter.Conditions.RevenueRate as RevenueRate exposing (RevenueRateCondition, RevenueRateMsg)
import Data.Filter.Conditions.SaleFee as SaleFee exposing (SaleFeeCondition, SaleFeeMsg)
import Data.Filter.Conditions.Story as Story exposing (StoryCondition, StoryMsg)
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
    }


type Condition
    = Condition_Amount AmountCondition
    | Condition_Elapsed_Term_Months ElapsedTermMonthsCondition
    | Condition_Elapsed_Term_Percent ElapsedTermPercentCondition
    | Condition_Health HealthCondition
    | Condition_Income IncomeCondition
    | Condition_Insurance InsuranceCondition
    | Condition_Interest InterestCondition
    | Condition_Loan_Annuity LoanAnnuityCondition
    | Condition_Purpose PurposeCondition
    | Condition_Relative_Profit RelativeProfitCondition
    | Condition_Remaining_Amount RemainingAmountCondition
    | Condition_Remaining_Term_Months RemainingTermMonthsCondition
    | Condition_Region RegionCondition
    | Condition_Revenue_Rate RevenueRateCondition
    | Condition_Story StoryCondition
    | Condition_Sale_Fee SaleFeeCondition
    | Condition_Term_Percent TermPercentCondition


type ConditionType
    = Amount
    | Elapsed_Term_Months
    | Elapsed_Term_Percent
    | Health
    | Income
    | Insurance
    | Interest
    | Loan_Annuity
    | Purpose
    | Region
    | Relative_Profit
    | Remaining_Amount
    | Remaining_Term_Months
    | Revenue_Rate
    | Sale_Fee
    | Story
    | Term_Percent


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
    }


renderCondition : Condition -> String
renderCondition condition =
    case condition of
        Condition_Amount c ->
            Amount.renderCondition c

        Condition_Elapsed_Term_Months c ->
            ElapsedTermMonths.renderCondition c

        Condition_Elapsed_Term_Percent c ->
            ElapsedTermPercent.renderCondition c

        Condition_Health c ->
            Health.renderCondition c

        Condition_Income c ->
            Income.renderCondition c

        Condition_Insurance c ->
            Insurance.renderCondition c

        Condition_Interest c ->
            Interest.renderCondition c

        Condition_Loan_Annuity c ->
            LoanAnnuity.renderCondition c

        Condition_Purpose c ->
            Purpose.renderCondition c

        Condition_Region c ->
            Region.renderCondition c

        Condition_Remaining_Amount c ->
            RemainingAmount.renderCondition c

        Condition_Remaining_Term_Months c ->
            RemainingTermMonths.renderCondition c

        Condition_Relative_Profit c ->
            RelativeProfit.renderCondition c

        Condition_Revenue_Rate c ->
            RevenueRate.renderCondition c

        Condition_Sale_Fee c ->
            SaleFee.renderCondition c

        Condition_Story c ->
            Story.renderCondition c

        Condition_Term_Percent c ->
            TermPercent.renderCondition c


conditionsValidationErrors : String -> Conditions -> List String
conditionsValidationErrors errorPrefix =
    List.map (\e -> errorPrefix ++ e) << List.concat << List.map conditionValidationError << getEnabledConditions


conditionValidationError : Condition -> List String
conditionValidationError condition =
    case condition of
        Condition_Amount c ->
            Amount.validationErrors c

        Condition_Elapsed_Term_Months c ->
            ElapsedTermMonths.validationErrors c

        Condition_Elapsed_Term_Percent c ->
            ElapsedTermPercent.validationErrors c

        Condition_Health c ->
            Health.validationErrors c

        Condition_Income c ->
            Income.validationErrors c

        Condition_Insurance _ ->
            [{- Insurance condition can't be invalid -> valid. errors list always empty -}]

        Condition_Interest c ->
            Interest.validationErrors c

        Condition_Loan_Annuity c ->
            LoanAnnuity.validationErrors c

        Condition_Purpose c ->
            Purpose.validationErrors c

        Condition_Region c ->
            Region.validationErrors c

        Condition_Remaining_Amount c ->
            RemainingAmount.validationErrors c

        Condition_Remaining_Term_Months c ->
            RemainingTermMonths.validationErrors c

        Condition_Relative_Profit c ->
            RelativeProfit.validationErrors c

        Condition_Revenue_Rate c ->
            RevenueRate.validationErrors c

        Condition_Sale_Fee _ ->
            [{- Sale fee condition can't be invalid -> valid. errors list always empty -}]

        Condition_Story _ ->
            [{- Story condition can't be invalid -> valid. errors list always empty -}]

        Condition_Term_Percent c ->
            TermPercent.validationErrors c


getDisabledConditionTypes : Conditions -> List ConditionType
getDisabledConditionTypes cs =
    let
        addIfNothing : (Conditions -> Maybe a) -> ConditionType -> List ConditionType
        addIfNothing field conditionType =
            Maybe.withDefault [ conditionType ] <| Maybe.map (always []) <| field cs
    in
    List.concat
        [ addIfNothing .amount Amount
        , addIfNothing .elapsedTermMonths Elapsed_Term_Months
        , addIfNothing .elapsedTermPercent Elapsed_Term_Percent
        , addIfNothing .health Health
        , addIfNothing .income Income
        , addIfNothing .insurance Insurance
        , addIfNothing .interest Interest
        , addIfNothing .loanAnnuity Loan_Annuity
        , addIfNothing .purpose Purpose
        , addIfNothing .region Region
        , addIfNothing .relativeProfit Relative_Profit
        , addIfNothing .remainingAmount Remaining_Amount
        , addIfNothing .remainingTermMonths Remaining_Term_Months
        , addIfNothing .revenueRate Revenue_Rate
        , addIfNothing .saleFee Sale_Fee
        , addIfNothing .story Story
        , addIfNothing .termPercent Term_Percent
        ]


getEnabledConditionTypes : Conditions -> List ConditionType
getEnabledConditionTypes cs =
    let
        addIfJust : (Conditions -> Maybe a) -> ConditionType -> List ConditionType
        addIfJust field conditionType =
            Maybe.withDefault [] <| Maybe.map (always [ conditionType ]) <| field cs
    in
    List.concat
        [ addIfJust .amount Amount
        , addIfJust .elapsedTermMonths Elapsed_Term_Months
        , addIfJust .elapsedTermPercent Elapsed_Term_Percent
        , addIfJust .health Health
        , addIfJust .income Income
        , addIfJust .insurance Insurance
        , addIfJust .interest Interest
        , addIfJust .loanAnnuity Loan_Annuity
        , addIfJust .purpose Purpose
        , addIfJust .region Region
        , addIfJust .relativeProfit Relative_Profit
        , addIfJust .remainingAmount Remaining_Amount
        , addIfJust .remainingTermMonths Remaining_Term_Months
        , addIfJust .revenueRate Revenue_Rate
        , addIfJust .saleFee Sale_Fee
        , addIfJust .story Story
        , addIfJust .termPercent Term_Percent
        ]


getEnabledConditions : Conditions -> List Condition
getEnabledConditions cs =
    let
        addIfJust : (Conditions -> Maybe a) -> (a -> Condition) -> List Condition
        addIfJust field wrap =
            Maybe.withDefault [] <| Maybe.map (List.singleton << wrap) <| field cs
    in
    List.concat
        [ addIfJust .amount Condition_Amount
        , addIfJust .elapsedTermMonths Condition_Elapsed_Term_Months
        , addIfJust .elapsedTermPercent Condition_Elapsed_Term_Percent
        , addIfJust .health Condition_Health
        , addIfJust .income Condition_Income
        , addIfJust .insurance Condition_Insurance
        , addIfJust .interest Condition_Interest
        , addIfJust .loanAnnuity Condition_Loan_Annuity
        , addIfJust .purpose Condition_Purpose
        , addIfJust .region Condition_Region
        , addIfJust .relativeProfit Condition_Relative_Profit
        , addIfJust .remainingAmount Condition_Remaining_Amount
        , addIfJust .remainingTermMonths Condition_Remaining_Term_Months
        , addIfJust .revenueRate Condition_Revenue_Rate
        , addIfJust .saleFee Condition_Sale_Fee
        , addIfJust .story Condition_Story
        , addIfJust .termPercent Condition_Term_Percent
        ]


addCondition : Condition -> Conditions -> Conditions
addCondition condition cs =
    case condition of
        Condition_Amount c ->
            { cs | amount = Just c }

        Condition_Elapsed_Term_Months c ->
            { cs | elapsedTermMonths = Just c }

        Condition_Elapsed_Term_Percent c ->
            { cs | elapsedTermPercent = Just c }

        Condition_Health c ->
            { cs | health = Just c }

        Condition_Income c ->
            { cs | income = Just c }

        Condition_Insurance c ->
            { cs | insurance = Just c }

        Condition_Interest c ->
            { cs | interest = Just c }

        Condition_Loan_Annuity c ->
            { cs | loanAnnuity = Just c }

        Condition_Purpose c ->
            { cs | purpose = Just c }

        Condition_Remaining_Amount c ->
            { cs | remainingAmount = Just c }

        Condition_Remaining_Term_Months c ->
            { cs | remainingTermMonths = Just c }

        Condition_Region c ->
            { cs | region = Just c }

        Condition_Relative_Profit c ->
            { cs | relativeProfit = Just c }

        Condition_Revenue_Rate c ->
            { cs | revenueRate = Just c }

        Condition_Sale_Fee c ->
            { cs | saleFee = Just c }

        Condition_Story c ->
            { cs | story = Just c }

        Condition_Term_Percent c ->
            { cs | termPercent = Just c }


updateInterest : InterestMsg -> Conditions -> Conditions
updateInterest msg conditions =
    { conditions | interest = Maybe.map (Interest.update msg) conditions.interest }


updateAmount : AmountMsg -> Conditions -> Conditions
updateAmount msg conditions =
    { conditions | amount = Maybe.map (Amount.update msg) conditions.amount }


updateRemainingAmount : RemainingAmountMsg -> Conditions -> Conditions
updateRemainingAmount msg conditions =
    { conditions | remainingAmount = Maybe.map (RemainingAmount.update msg) conditions.remainingAmount }


updateRevenueRate : RevenueRateMsg -> Conditions -> Conditions
updateRevenueRate msg conditions =
    { conditions | revenueRate = Maybe.map (RevenueRate.update msg) conditions.revenueRate }


updateSaleFee : SaleFeeMsg -> Conditions -> Conditions
updateSaleFee msg conditions =
    { conditions | saleFee = Maybe.map (SaleFee.update msg) conditions.saleFee }


updateStory : StoryMsg -> Conditions -> Conditions
updateStory msg conditions =
    { conditions | story = Maybe.map (Story.update msg) conditions.story }


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


updateInsurance : InsuranceMsg -> Conditions -> Conditions
updateInsurance msg conditions =
    { conditions | insurance = Maybe.map (Insurance.update msg) conditions.insurance }


updateLoanAnnuity : LoanAnnuityMsg -> Conditions -> Conditions
updateLoanAnnuity msg conditions =
    { conditions | loanAnnuity = Maybe.map (LoanAnnuity.update msg) conditions.loanAnnuity }


removeCondition : ConditionType -> Conditions -> Conditions
removeCondition conditionType cs =
    case conditionType of
        Amount ->
            { cs | amount = Nothing }

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

        Loan_Annuity ->
            { cs | loanAnnuity = Nothing }

        Purpose ->
            { cs | purpose = Nothing }

        Region ->
            { cs | region = Nothing }

        Relative_Profit ->
            { cs | relativeProfit = Nothing }

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

        Loan_Annuity ->
            Condition_Loan_Annuity LoanAnnuity.defaultCondition

        Purpose ->
            Condition_Purpose Purpose.defaultCondition

        Region ->
            Condition_Region Region.defaultCondition

        Relative_Profit ->
            Condition_Relative_Profit RelativeProfit.defaultCondition

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



-- JSON


encodeConditions : Conditions -> Value
encodeConditions conditions =
    getEnabledConditions conditions
        |> List.map encodeCondition
        |> Encode.object


encodeCondition : Condition -> ( String, Value )
encodeCondition condition =
    case condition of
        Condition_Region c ->
            ( "A", Region.encodeCondition c )

        -- Condition_Rating c ->
        --     ( "B", Rating.encodeCondition c )
        Condition_Income c ->
            ( "C", Income.encodeCondition c )

        Condition_Purpose c ->
            ( "D", Purpose.encodeCondition c )

        Condition_Story c ->
            ( "E", Story.encodeCondition c )

        Condition_Remaining_Term_Months c ->
            ( "F", RemainingTermMonths.encodeCondition c )

        Condition_Term_Percent c ->
            ( "G", TermPercent.encodeCondition c )

        Condition_Elapsed_Term_Months c ->
            ( "H", ElapsedTermMonths.encodeCondition c )

        Condition_Elapsed_Term_Percent c ->
            ( "I", ElapsedTermPercent.encodeCondition c )

        Condition_Interest c ->
            ( "J", Interest.encodeCondition c )

        Condition_Amount c ->
            ( "K", Amount.encodeCondition c )

        Condition_Insurance c ->
            ( "L", Insurance.encodeCondition c )

        Condition_Remaining_Amount c ->
            ( "M", RemainingAmount.encodeCondition c )

        Condition_Loan_Annuity c ->
            ( "N", LoanAnnuity.encodeCondition c )

        Condition_Revenue_Rate c ->
            ( "O", RevenueRate.encodeCondition c )

        Condition_Sale_Fee c ->
            ( "P", SaleFee.encodeCondition c )

        Condition_Relative_Profit c ->
            ( "Q", RelativeProfit.encodeCondition c )

        Condition_Health c ->
            ( "R", Health.encodeCondition c )


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
