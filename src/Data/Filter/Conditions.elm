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
    , updateIncome
    , updateInsurance
    , updateInterest
    , updatePurpose
    , updateRating
    , updateRegion
    , updateRemainingAmount
    , updateStory
    , updateTermMonths
    , updateTermPercent
    )

import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition, AmountMsg)
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonthsCondition, ElapsedTermMonthsMsg)
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercentCondition, ElapsedTermPercentMsg)
import Data.Filter.Conditions.Income as Income exposing (IncomeCondition, IncomeMsg)
import Data.Filter.Conditions.Insurance as Insurance exposing (InsuranceCondition, InsuranceMsg)
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition, InterestMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (PurposeCondition, PurposeMsg)
import Data.Filter.Conditions.Rating as Rating exposing (RatingCondition, RatingMsg)
import Data.Filter.Conditions.Region as Region exposing (RegionCondition, RegionMsg)
import Data.Filter.Conditions.RemainingAmount as RemainingAmount exposing (RemainingAmountCondition, RemainingAmountMsg)
import Data.Filter.Conditions.Story as Story exposing (StoryCondition, StoryMsg)
import Data.Filter.Conditions.TermMonths as TermMonths exposing (TermMonthsCondition, TermMonthsMsg)
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercentCondition, TermPercentMsg)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing (andMap, optionalField)
import Json.Encode as Encode exposing (Value)
import List


type alias Conditions =
    { region : Maybe RegionCondition
    , rating : Maybe RatingCondition
    , income : Maybe IncomeCondition
    , purpose : Maybe PurposeCondition
    , story : Maybe StoryCondition
    , termMonths : Maybe TermMonthsCondition
    , termPercent : Maybe TermPercentCondition
    , elapsedTermMonths : Maybe ElapsedTermMonthsCondition
    , elapsedTermPercent : Maybe ElapsedTermPercentCondition
    , interest : Maybe InterestCondition
    , amount : Maybe AmountCondition
    , insurance : Maybe InsuranceCondition
    , remainingAmount : Maybe RemainingAmountCondition
    }


type Condition
    = Condition_Amount AmountCondition
    | Condition_Elapsed_Term_Months ElapsedTermMonthsCondition
    | Condition_Elapsed_Term_Percent ElapsedTermPercentCondition
    | Condition_Income IncomeCondition
    | Condition_Insurance InsuranceCondition
    | Condition_Interest InterestCondition
    | Condition_Purpose PurposeCondition
    | Condition_Rating RatingCondition
    | Condition_Remaining_Amount RemainingAmountCondition
    | Condition_Region RegionCondition
    | Condition_Story StoryCondition
    | Condition_Term_Months TermMonthsCondition
    | Condition_Term_Percent TermPercentCondition


type ConditionType
    = Amount
    | Elapsed_Term_Months
    | Elapsed_Term_Percent
    | Income
    | Insurance
    | Interest
    | Purpose
    | Rating
    | Remaining_Amount
    | Region
    | Story
    | Term_Months
    | Term_Percent


emptyConditions : Conditions
emptyConditions =
    { region = Nothing
    , rating = Nothing
    , income = Nothing
    , purpose = Nothing
    , story = Nothing
    , termMonths = Nothing
    , termPercent = Nothing
    , elapsedTermMonths = Nothing
    , elapsedTermPercent = Nothing
    , interest = Nothing
    , amount = Nothing
    , insurance = Nothing
    , remainingAmount = Nothing
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

        Condition_Income c ->
            Income.renderCondition c

        Condition_Insurance c ->
            Insurance.renderCondition c

        Condition_Interest c ->
            Interest.renderCondition c

        Condition_Purpose c ->
            Purpose.renderCondition c

        Condition_Rating c ->
            Rating.renderCondition c

        Condition_Region c ->
            Region.renderCondition c

        Condition_Remaining_Amount c ->
            RemainingAmount.renderCondition c

        Condition_Story c ->
            Story.renderCondition c

        Condition_Term_Months c ->
            TermMonths.renderCondition c

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

        Condition_Income c ->
            Income.validationErrors c

        Condition_Insurance _ ->
            [{- Insurance condition can't be invalid -> valid. errors list always empty -}]

        Condition_Interest c ->
            Interest.validationErrors c

        Condition_Purpose c ->
            Purpose.validationErrors c

        Condition_Rating c ->
            Rating.validationErrors c

        Condition_Region c ->
            Region.validationErrors c

        Condition_Remaining_Amount c ->
            RemainingAmount.validationErrors c

        Condition_Story _ ->
            [{- Story condition can't be invalid -> valid. errors list always empty -}]

        Condition_Term_Months c ->
            TermMonths.validationErrors c

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
        , addIfNothing .income Income
        , addIfNothing .insurance Insurance
        , addIfNothing .interest Interest
        , addIfNothing .purpose Purpose
        , addIfNothing .rating Rating
        , addIfNothing .remainingAmount Remaining_Amount
        , addIfNothing .region Region
        , addIfNothing .story Story
        , addIfNothing .termMonths Term_Months
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
        , addIfJust .income Income
        , addIfJust .insurance Insurance
        , addIfJust .interest Interest
        , addIfJust .purpose Purpose
        , addIfJust .rating Rating
        , addIfJust .region Region
        , addIfJust .remainingAmount Remaining_Amount
        , addIfJust .story Story
        , addIfJust .termMonths Term_Months
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
        , addIfJust .income Condition_Income
        , addIfJust .insurance Condition_Insurance
        , addIfJust .interest Condition_Interest
        , addIfJust .purpose Condition_Purpose
        , addIfJust .rating Condition_Rating
        , addIfJust .region Condition_Region
        , addIfJust .remainingAmount Condition_Remaining_Amount
        , addIfJust .story Condition_Story
        , addIfJust .termMonths Condition_Term_Months
        , addIfJust .termPercent Condition_Term_Percent
        ]


addCondition : Condition -> Conditions -> Conditions
addCondition condition cs =
    case condition of
        Condition_Amount c ->
            setAmountCondition c cs

        Condition_Elapsed_Term_Months c ->
            setElapsedTermMonthsCondition c cs

        Condition_Elapsed_Term_Percent c ->
            setElapsedTermPercentCondition c cs

        Condition_Income c ->
            setIncomeCondition c cs

        Condition_Insurance c ->
            setInsuranceCondition c cs

        Condition_Interest c ->
            setInterestCondition c cs

        Condition_Purpose c ->
            setPurposeCondition c cs

        Condition_Rating c ->
            setRatingCondition c cs

        Condition_Remaining_Amount c ->
            setRemainingAmountCondition c cs

        Condition_Region c ->
            setRegionCondition c cs

        Condition_Story c ->
            setStoryCondition c cs

        Condition_Term_Months c ->
            setTermMonthsCondition c cs

        Condition_Term_Percent c ->
            setTermPercentCondition c cs


setRegionCondition : RegionCondition -> Conditions -> Conditions
setRegionCondition c cs =
    { cs | region = Just c }


setRatingCondition : RatingCondition -> Conditions -> Conditions
setRatingCondition c cs =
    { cs | rating = Just c }


setIncomeCondition : IncomeCondition -> Conditions -> Conditions
setIncomeCondition c cs =
    { cs | income = Just c }


setPurposeCondition : PurposeCondition -> Conditions -> Conditions
setPurposeCondition c cs =
    { cs | purpose = Just c }


setStoryCondition : StoryCondition -> Conditions -> Conditions
setStoryCondition c cs =
    { cs | story = Just c }


setTermMonthsCondition : TermMonthsCondition -> Conditions -> Conditions
setTermMonthsCondition c cs =
    { cs | termMonths = Just c }


setTermPercentCondition : TermPercentCondition -> Conditions -> Conditions
setTermPercentCondition c cs =
    { cs | termPercent = Just c }


setElapsedTermMonthsCondition : ElapsedTermMonthsCondition -> Conditions -> Conditions
setElapsedTermMonthsCondition c cs =
    { cs | elapsedTermMonths = Just c }


setElapsedTermPercentCondition : ElapsedTermPercentCondition -> Conditions -> Conditions
setElapsedTermPercentCondition c cs =
    { cs | elapsedTermPercent = Just c }


setInterestCondition : InterestCondition -> Conditions -> Conditions
setInterestCondition c cs =
    { cs | interest = Just c }


setAmountCondition : AmountCondition -> Conditions -> Conditions
setAmountCondition c cs =
    { cs | amount = Just c }


setInsuranceCondition : InsuranceCondition -> Conditions -> Conditions
setInsuranceCondition c cs =
    { cs | insurance = Just c }


setRemainingAmountCondition : RemainingAmountCondition -> Conditions -> Conditions
setRemainingAmountCondition c cs =
    { cs | remainingAmount = Just c }


updateInterest : InterestMsg -> Conditions -> Conditions
updateInterest msg conditions =
    { conditions | interest = Maybe.map (Interest.update msg) conditions.interest }


updateAmount : AmountMsg -> Conditions -> Conditions
updateAmount msg conditions =
    { conditions | amount = Maybe.map (Amount.update msg) conditions.amount }


updateRemainingAmount : RemainingAmountMsg -> Conditions -> Conditions
updateRemainingAmount msg conditions =
    { conditions | remainingAmount = Maybe.map (RemainingAmount.update msg) conditions.remainingAmount }


updateStory : StoryMsg -> Conditions -> Conditions
updateStory msg conditions =
    { conditions | story = Maybe.map (Story.update msg) conditions.story }


updatePurpose : PurposeMsg -> Conditions -> Conditions
updatePurpose msg conditions =
    { conditions | purpose = Maybe.map (Purpose.update msg) conditions.purpose }


updateTermMonths : TermMonthsMsg -> Conditions -> Conditions
updateTermMonths msg conditions =
    { conditions | termMonths = Maybe.map (TermMonths.update msg) conditions.termMonths }


updateTermPercent : TermPercentMsg -> Conditions -> Conditions
updateTermPercent msg conditions =
    { conditions | termPercent = Maybe.map (TermPercent.update msg) conditions.termPercent }


updateElapsedTermMonths : ElapsedTermMonthsMsg -> Conditions -> Conditions
updateElapsedTermMonths msg conditions =
    { conditions | elapsedTermMonths = Maybe.map (ElapsedTermMonths.update msg) conditions.elapsedTermMonths }


updateElapsedTermPercent : ElapsedTermPercentMsg -> Conditions -> Conditions
updateElapsedTermPercent msg conditions =
    { conditions | elapsedTermPercent = Maybe.map (ElapsedTermPercent.update msg) conditions.elapsedTermPercent }


updateIncome : IncomeMsg -> Conditions -> Conditions
updateIncome msg conditions =
    { conditions | income = Maybe.map (Income.update msg) conditions.income }


updateRating : RatingMsg -> Conditions -> Conditions
updateRating msg conditions =
    { conditions | rating = Maybe.map (Rating.update msg) conditions.rating }


updateRegion : RegionMsg -> Conditions -> Conditions
updateRegion msg conditions =
    { conditions | region = Maybe.map (Region.update msg) conditions.region }


updateInsurance : InsuranceMsg -> Conditions -> Conditions
updateInsurance msg conditions =
    { conditions | insurance = Maybe.map (Insurance.update msg) conditions.insurance }


removeCondition : ConditionType -> Conditions -> Conditions
removeCondition conditionType cs =
    case conditionType of
        Amount ->
            { cs | amount = Nothing }

        Elapsed_Term_Months ->
            { cs | elapsedTermMonths = Nothing }

        Elapsed_Term_Percent ->
            { cs | elapsedTermPercent = Nothing }

        Income ->
            { cs | income = Nothing }

        Insurance ->
            { cs | insurance = Nothing }

        Interest ->
            { cs | interest = Nothing }

        Purpose ->
            { cs | purpose = Nothing }

        Rating ->
            { cs | rating = Nothing }

        Region ->
            { cs | region = Nothing }

        Remaining_Amount ->
            { cs | remainingAmount = Nothing }

        Story ->
            { cs | story = Nothing }

        Term_Months ->
            { cs | termMonths = Nothing }

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

        Income ->
            Condition_Income Income.defaultCondition

        Insurance ->
            Condition_Insurance Insurance.defaultCondition

        Interest ->
            Condition_Interest Interest.defaultCondition

        Purpose ->
            Condition_Purpose Purpose.defaultCondition

        Rating ->
            Condition_Rating Rating.defaultCondition

        Region ->
            Condition_Region Region.defaultCondition

        Remaining_Amount ->
            Condition_Remaining_Amount RemainingAmount.defaultCondition

        Story ->
            Condition_Story Story.defaultCondition

        Term_Months ->
            Condition_Term_Months TermMonths.defaultCondition

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

        Condition_Rating c ->
            ( "B", Rating.encodeCondition c )

        Condition_Income c ->
            ( "C", Income.encodeCondition c )

        Condition_Purpose c ->
            ( "D", Purpose.encodeCondition c )

        Condition_Story c ->
            ( "E", Story.encodeCondition c )

        Condition_Term_Months c ->
            ( "F", TermMonths.encodeCondition c )

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


conditionsDecoder : Decoder Conditions
conditionsDecoder =
    Decode.succeed Conditions
        |> andMap (optionalField "A" Region.conditionDecoder)
        |> andMap (optionalField "B" Rating.conditionDecoder)
        |> andMap (optionalField "C" Income.conditionDecoder)
        |> andMap (optionalField "D" Purpose.conditionDecoder)
        |> andMap (optionalField "E" Story.conditionDecoder)
        |> andMap (optionalField "F" TermMonths.conditionDecoder)
        |> andMap (optionalField "G" TermPercent.conditionDecoder)
        |> andMap (optionalField "H" ElapsedTermMonths.conditionDecoder)
        |> andMap (optionalField "I" ElapsedTermPercent.conditionDecoder)
        |> andMap (optionalField "J" Interest.conditionDecoder)
        |> andMap (optionalField "K" Amount.conditionDecoder)
        |> andMap (optionalField "L" Insurance.conditionDecoder)
        |> andMap (optionalField "M" RemainingAmount.conditionDecoder)
