module Data.Filter.Conditions
    exposing
        ( Condition(..)
        , Conditions
        , addCondition
        , conditionsDecoder
        , conditionsToList
        , conditionsValidationErrors
        , emptyConditions
        , encodeConditions
        , removeAmountCondition
        , removeElapsedTermMonthsCondition
        , removeElapsedTermPercentCondition
        , removeInterestCondition
        , removeMainIncomeCondition
        , removePurposeCondition
        , removeRatingCondition
        , removeRegionCondition
        , removeStoryCondition
        , removeTermMonthsCondition
        , removeTermPercentCondition
        , renderCondition
        , updateAmount
        , updateElapsedTermMonths
        , updateElapsedTermPercent
        , updateInterest
        , updateMainIncome
        , updatePurpose
        , updateRating
        , updateRegion
        , updateStory
        , updateTermMonths
        , updateTermPercent
        )

import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition, AmountMsg)
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonthsCondition, ElapsedTermMonthsMsg)
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercentCondition, ElapsedTermPercentMsg)
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition, InterestMsg)
import Data.Filter.Conditions.MainIncome as MainIncome exposing (MainIncomeCondition, MainIncomeMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (PurposeCondition, PurposeMsg)
import Data.Filter.Conditions.Rating as Rating exposing (RatingCondition, RatingMsg)
import Data.Filter.Conditions.Region as Region exposing (RegionCondition, RegionMsg)
import Data.Filter.Conditions.Story as Story exposing (StoryCondition, StoryMsg)
import Data.Filter.Conditions.TermMonths as TermMonths exposing (TermMonthsCondition, TermMonthsMsg)
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercentCondition, TermPercentMsg)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing ((|:), optionalField)
import Json.Encode as Encode exposing (Value)


type alias Conditions =
    { region : Maybe RegionCondition
    , rating : Maybe RatingCondition
    , income : Maybe MainIncomeCondition
    , purpose : Maybe PurposeCondition
    , story : Maybe StoryCondition
    , termMonths : Maybe TermMonthsCondition
    , termPercent : Maybe TermPercentCondition
    , elapsedTermMonths : Maybe ElapsedTermMonthsCondition
    , elapsedTermPercent : Maybe ElapsedTermPercentCondition
    , interest : Maybe InterestCondition
    , amount : Maybe AmountCondition
    }


type Condition
    = Condition_Region RegionCondition
    | Condition_Rating RatingCondition
    | Condition_Income MainIncomeCondition
    | Condition_Purpose PurposeCondition
    | Condition_Story StoryCondition
    | Condition_Term_Months TermMonthsCondition
    | Condition_Term_Percent TermPercentCondition
    | Condition_Elapsed_Term_Months ElapsedTermMonthsCondition
    | Condition_Elapsed_Term_Percent ElapsedTermPercentCondition
    | Condition_Interest InterestCondition
    | Condition_Amount AmountCondition


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
    }


renderCondition : Condition -> String
renderCondition condition =
    case condition of
        Condition_Region c ->
            Region.renderCondition c

        Condition_Rating c ->
            Rating.renderCondition c

        Condition_Income c ->
            MainIncome.renderCondition c

        Condition_Purpose c ->
            Purpose.renderCondition c

        Condition_Story c ->
            Story.renderCondition c

        Condition_Term_Months c ->
            TermMonths.renderCondition c

        Condition_Term_Percent c ->
            TermPercent.renderCondition c

        Condition_Elapsed_Term_Months c ->
            ElapsedTermMonths.renderCondition c

        Condition_Elapsed_Term_Percent c ->
            ElapsedTermPercent.renderCondition c

        Condition_Interest c ->
            Interest.renderCondition c

        Condition_Amount c ->
            Amount.renderCondition c


conditionsValidationErrors : String -> Conditions -> List String
conditionsValidationErrors errorPrefix =
    List.map (\e -> errorPrefix ++ e) << List.concat << List.map conditionValidationError << conditionsToList


conditionValidationError : Condition -> List String
conditionValidationError c =
    case c of
        Condition_Region regionCond ->
            Region.validationErrors regionCond

        Condition_Rating ratingCond ->
            Rating.validationErrors ratingCond

        Condition_Income incomeCond ->
            MainIncome.validationErrors incomeCond

        Condition_Purpose purposeCond ->
            Purpose.validationErrors purposeCond

        Condition_Story _ ->
            [{- Story condition can't be invalid -> valid. errors list always empty -}]

        Condition_Term_Months termMonthsCond ->
            TermMonths.validationErrors termMonthsCond

        Condition_Term_Percent termPercentCond ->
            TermPercent.validationErrors termPercentCond

        Condition_Elapsed_Term_Months elapsedTermMonthsCond ->
            ElapsedTermMonths.validationErrors elapsedTermMonthsCond

        Condition_Elapsed_Term_Percent elapsedTermPercentCond ->
            ElapsedTermPercent.validationErrors elapsedTermPercentCond

        Condition_Interest interestCond ->
            Interest.validationErrors interestCond

        Condition_Amount amountCond ->
            Amount.validationErrors amountCond


conditionsToList : Conditions -> List Condition
conditionsToList { region, rating, income, purpose, story, termMonths, termPercent, elapsedTermMonths, elapsedTermPercent, amount, interest } =
    let
        fromMaybe : (a -> Condition) -> Maybe a -> List Condition
        fromMaybe wrap =
            Maybe.withDefault [] << Maybe.map (List.singleton << wrap)

        reg =
            fromMaybe Condition_Region region

        rat =
            fromMaybe Condition_Rating rating

        inc =
            fromMaybe Condition_Income income

        pur =
            fromMaybe Condition_Purpose purpose

        sto =
            fromMaybe Condition_Story story

        terM =
            fromMaybe Condition_Term_Months termMonths

        terP =
            fromMaybe Condition_Term_Percent termPercent

        elTermM =
            fromMaybe Condition_Elapsed_Term_Months elapsedTermMonths

        elTermP =
            fromMaybe Condition_Elapsed_Term_Percent elapsedTermPercent

        inte =
            fromMaybe Condition_Interest interest

        amo =
            fromMaybe Condition_Amount amount
    in
    List.concat [ reg, rat, inc, pur, sto, terM, terP, elTermM, elTermP, inte, amo ]


addCondition : Condition -> Conditions -> Conditions
addCondition c cs =
    case c of
        Condition_Region regionCond ->
            setRegionCondition regionCond cs

        Condition_Rating ratingCond ->
            setRatingCondition ratingCond cs

        Condition_Income incomeCond ->
            setIncomeCondition incomeCond cs

        Condition_Purpose purposeCond ->
            setPurposeCondition purposeCond cs

        Condition_Story storyCond ->
            setStoryCondition storyCond cs

        Condition_Term_Months termMonthsCond ->
            setTermMonthsCondition termMonthsCond cs

        Condition_Term_Percent termPercentCond ->
            setTermPercentCondition termPercentCond cs

        Condition_Elapsed_Term_Months elapsedTermMonthsCond ->
            setElapsedTermMonthsCondition elapsedTermMonthsCond cs

        Condition_Elapsed_Term_Percent elapsedTermPercentCond ->
            setElapsedTermPercentCondition elapsedTermPercentCond cs

        Condition_Interest interestCond ->
            setInterestCondition interestCond cs

        Condition_Amount amountCond ->
            setAmountCondition amountCond cs


setRegionCondition : RegionCondition -> Conditions -> Conditions
setRegionCondition c cs =
    { cs | region = Just c }


setRatingCondition : RatingCondition -> Conditions -> Conditions
setRatingCondition c cs =
    { cs | rating = Just c }


setIncomeCondition : MainIncomeCondition -> Conditions -> Conditions
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


updateInterest : InterestMsg -> Conditions -> Conditions
updateInterest msg conditions =
    { conditions | interest = Maybe.map (Interest.update msg) conditions.interest }


updateAmount : AmountMsg -> Conditions -> Conditions
updateAmount msg conditions =
    { conditions | amount = Maybe.map (Amount.update msg) conditions.amount }


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


updateMainIncome : MainIncomeMsg -> Conditions -> Conditions
updateMainIncome msg conditions =
    { conditions | income = Maybe.map (MainIncome.update msg) conditions.income }


updateRating : RatingMsg -> Conditions -> Conditions
updateRating msg conditions =
    { conditions | rating = Maybe.map (Rating.update msg) conditions.rating }


updateRegion : RegionMsg -> Conditions -> Conditions
updateRegion msg conditions =
    { conditions | region = Maybe.map (Region.update msg) conditions.region }


removeAmountCondition : Conditions -> Conditions
removeAmountCondition cs =
    { cs | amount = Nothing }


removeStoryCondition : Conditions -> Conditions
removeStoryCondition cs =
    { cs | story = Nothing }


removeInterestCondition : Conditions -> Conditions
removeInterestCondition cs =
    { cs | interest = Nothing }


removePurposeCondition : Conditions -> Conditions
removePurposeCondition cs =
    { cs | purpose = Nothing }


removeTermMonthsCondition : Conditions -> Conditions
removeTermMonthsCondition cs =
    { cs | termMonths = Nothing }


removeTermPercentCondition : Conditions -> Conditions
removeTermPercentCondition cs =
    { cs | termPercent = Nothing }


removeElapsedTermMonthsCondition : Conditions -> Conditions
removeElapsedTermMonthsCondition cs =
    { cs | elapsedTermMonths = Nothing }


removeElapsedTermPercentCondition : Conditions -> Conditions
removeElapsedTermPercentCondition cs =
    { cs | elapsedTermPercent = Nothing }


removeMainIncomeCondition : Conditions -> Conditions
removeMainIncomeCondition cs =
    { cs | income = Nothing }


removeRatingCondition : Conditions -> Conditions
removeRatingCondition cs =
    { cs | rating = Nothing }


removeRegionCondition : Conditions -> Conditions
removeRegionCondition cs =
    { cs | region = Nothing }



-- JSON


encodeConditions : Conditions -> Value
encodeConditions conditions =
    conditionsToList conditions
        |> List.map encodeCondition
        |> Encode.object


encodeCondition : Condition -> ( String, Value )
encodeCondition c =
    case c of
        Condition_Region regionCond ->
            ( "A", Region.encodeCondition regionCond )

        Condition_Rating ratingCond ->
            ( "B", Rating.encodeCondition ratingCond )

        Condition_Income incomeCond ->
            ( "C", MainIncome.encodeCondition incomeCond )

        Condition_Purpose purposeCond ->
            ( "D", Purpose.encodeCondition purposeCond )

        Condition_Story storyCond ->
            ( "E", Story.encodeCondition storyCond )

        Condition_Term_Months termMonthsCond ->
            ( "F", TermMonths.encodeCondition termMonthsCond )

        Condition_Term_Percent termPercentCond ->
            ( "G", TermPercent.encodeCondition termPercentCond )

        Condition_Elapsed_Term_Months elapsedTermMonthsCond ->
            ( "H", ElapsedTermMonths.encodeCondition elapsedTermMonthsCond )

        Condition_Elapsed_Term_Percent elapsedTermPercentCond ->
            ( "I", ElapsedTermPercent.encodeCondition elapsedTermPercentCond )

        Condition_Interest interestCond ->
            ( "J", Interest.encodeCondition interestCond )

        Condition_Amount amountCond ->
            ( "K", Amount.encodeCondition amountCond )


conditionsDecoder : Decoder Conditions
conditionsDecoder =
    Decode.succeed Conditions
        |: optionalField "A" Region.conditionDecoder
        |: optionalField "B" Rating.conditionDecoder
        |: optionalField "C" MainIncome.conditionDecoder
        |: optionalField "D" Purpose.conditionDecoder
        |: optionalField "E" Story.conditionDecoder
        |: optionalField "F" TermMonths.conditionDecoder
        |: optionalField "G" TermPercent.conditionDecoder
        |: optionalField "H" ElapsedTermMonths.conditionDecoder
        |: optionalField "I" ElapsedTermPercent.conditionDecoder
        |: optionalField "J" Interest.conditionDecoder
        |: optionalField "K" Amount.conditionDecoder
