module Test.RandomStrategy exposing (..)

import AllDict
import Data.Confirmation exposing (ConfirmationSettings)
import Data.Filter exposing (FilteredItem(..), MarketplaceFilter(..))
import Data.Filter.Conditions exposing (Condition(..), Conditions, addCondition, emptyConditions)
import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition(AmountCondition))
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition(..))
import Data.Filter.Conditions.LoanPurpose as LoanPurpose exposing (LoanPurposeCondition(LoanPurposeList))
import Data.Filter.Conditions.LoanTerm as LoanTerm exposing (LoanTermCondition(..))
import Data.Filter.Conditions.MainIncome as MainIncome exposing (MainIncomeCondition(..))
import Data.Filter.Conditions.Rating as Rating exposing (RatingCondition(RatingList))
import Data.Filter.Conditions.Region as Region exposing (RegionCondition(RegionList))
import Data.Filter.Conditions.Story exposing (Story(..), StoryCondition(StoryCondition))
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare(..))
import Data.Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration)
import Data.TargetBalance as TargetBalance exposing (TargetBalance(..))
import Data.TargetPortfolioSize as TargetPortfoliSize exposing (TargetPortfolioSize(TargetPortfolioSize))
import Random exposing (Generator)
import Random.Extra as Random
import Random.List


strategyConfigurationGen : Generator StrategyConfiguration
strategyConfigurationGen =
    Random.map5 StrategyConfiguration
        generalSettingsGen
        portfolioSharesGen
        investmentsPerRatingGen
        buyFiltersGen
        sellFiltersGen


generalSettingsGen : Generator GeneralSettings
generalSettingsGen =
    Random.map6 GeneralSettings
        portfolioGen
        targetPortfolioSizeGen
        investmentSizeGen
        investmentShareGen
        targetBalanceGen
        confirmationSettingsGen


portfolioSharesGen : Generator PortfolioShares
portfolioSharesGen =
    eightIntsThatAddUpTo100
        |> Random.andThen
            (\minimumShares ->
                List.map (\from -> percentageFrom from) minimumShares
                    |> Random.combine
            )
        |> Random.map
            (\sharesList ->
                List.map2 (\rtg ( from, to ) -> ( rtg, PortfolioStructure.percentageShare from to )) Rating.allRatings sharesList
                    |> AllDict.fromList Rating.hash
            )


investmentsPerRatingGen : Generator InvestmentsPerRating
investmentsPerRatingGen =
    Random.list (List.length Rating.allRatings) investment0to5kRange
        |> Random.map
            (\investmentSizes ->
                List.map2 (\rtg sz -> ( rtg, sz )) Rating.allRatings investmentSizes
                    |> AllDict.fromList Rating.hash
            )


investment0to5kRange : Generator Investment.Size
investment0to5kRange =
    Random.int 0 25 |> Random.andThen (\from -> Random.int from 25 |> Random.map (\to -> Investment.size (200 * from) (200 * to)))


buyFiltersGen : Generator (List MarketplaceFilter)
buyFiltersGen =
    Random.rangeLengthList 0 10 buyFilterGen


sellFiltersGen : Generator (List MarketplaceFilter)
sellFiltersGen =
    Random.rangeLengthList 0 10 sellFilterGen


buyFilterGen : Generator MarketplaceFilter
buyFilterGen =
    filteredItemGen
        |> Random.andThen filterGen


sellFilterGen : Generator MarketplaceFilter
sellFilterGen =
    filterGen Participation_To_Sell


filterGen : FilteredItem -> Generator MarketplaceFilter
filterGen filteredItem =
    let
        conditionsGen =
            case filteredItem of
                Loan ->
                    loanConditionsGen

                _ ->
                    participationConditionsGen
    in
    Random.map2 (\pos neg -> MarketplaceFilter { whatToFilter = filteredItem, ignoreWhen = pos, butNotWhen = neg })
        (conditionsGen 1)
        (conditionsGen 0)


loanConditionsGen : Int -> Generator Conditions
loanConditionsGen minimumConditions =
    subset minimumConditions
        (amountConditionGen :: conditionsSharedByAllFilteredItems)
        |> Random.andThen (Random.combine >> Random.map (List.foldl addCondition emptyConditions))


participationConditionsGen : Int -> Generator Conditions
participationConditionsGen minimumConditions =
    subset minimumConditions
        conditionsSharedByAllFilteredItems
        |> Random.andThen (Random.combine >> Random.map (List.foldl addCondition emptyConditions))


conditionsSharedByAllFilteredItems : List (Generator Condition)
conditionsSharedByAllFilteredItems =
    [ regionConditionGen
    , ratingConditionGen
    , incomeConditionGen
    , purposeConditionGen
    , storyConditionGen
    , termConditionGen
    , interestConditionGen
    ]


regionConditionGen : Generator Condition
regionConditionGen =
    nonemptySubset Region.allRegions |> Random.map (RegionList >> Condition_Region)


ratingConditionGen : Generator Condition
ratingConditionGen =
    nonemptySubset Rating.allRatings |> Random.map (RatingList >> Condition_Rating)


incomeConditionGen : Generator Condition
incomeConditionGen =
    nonemptySubset MainIncome.allIncomes |> Random.map (MainIncomeList >> Condition_Income)


purposeConditionGen : Generator Condition
purposeConditionGen =
    nonemptySubset LoanPurpose.allPurposes |> Random.map (LoanPurposeList >> Condition_Purpose)


storyConditionGen : Generator Condition
storyConditionGen =
    Random.sample [ SHORT, BELOW_AVERAGE, AVERAGE, ABOVE_AVERAGE ]
        |> Random.map (Maybe.withDefault SHORT >> StoryCondition >> Condition_Story)


termConditionGen : Generator Condition
termConditionGen =
    let
        minLoanTerm =
            0

        maxLoanTerm =
            84
    in
    Random.choices
        [ Random.map LoanTerm.LessThan (Random.int (minLoanTerm + 1 {- 0 is invalid, as parser subtract 1 -}) (maxLoanTerm + 1))
        , Random.int minLoanTerm maxLoanTerm |> Random.andThen (\mi -> Random.int mi maxLoanTerm |> Random.map (\mx -> LoanTerm.Between mi mx))
        , Random.map LoanTerm.MoreThan (Random.int minLoanTerm (maxLoanTerm - 1 {- max is invalid, as parser adds 1 -}))
        ]
        |> Random.map (LoanTermCondition >> Condition_Term)


amountConditionGen : Generator Condition
amountConditionGen =
    let
        maxAmount =
            1000000
    in
    Random.choices
        [ Random.map Amount.LessThan (Random.int 0 maxAmount)
        , Random.int 0 maxAmount |> Random.andThen (\mi -> Random.int mi maxAmount |> Random.map (\mx -> Amount.Between mi mx))
        , Random.map Amount.MoreThan (Random.int 0 maxAmount)
        ]
        |> Random.map (AmountCondition >> Condition_Amount)


interestConditionGen : Generator Condition
interestConditionGen =
    Random.choices
        [ Random.map Interest.LessThan (Random.float 0 100)
        , Random.float 0 100 |> Random.andThen (\mi -> Random.float mi 100 |> Random.map (\mx -> Interest.Between mi mx))
        , Random.map Interest.MoreThan (Random.float 0 100)
        ]
        |> Random.map (InterestCondition >> Condition_Interest)


filteredItemGen : Generator FilteredItem
filteredItemGen =
    {- Intentionally NOT generating Participation_To_Sell - we're only generating items for BUY filters -}
    Random.sample [ Loan, Participation, Loan_And_Participation ]
        |> Random.map (Maybe.withDefault Loan)


portfolioGen : Generator Portfolio
portfolioGen =
    Random.sample [ Conservative, Balanced, Progressive, Empty ]
        |> Random.map (Maybe.withDefault Conservative)


targetPortfolioSizeGen : Generator TargetPortfolioSize
targetPortfolioSizeGen =
    Random.frequency
        [ ( 1, Random.constant TargetPortfoliSize.NotSpecified )
        , ( 2, Random.int 0 1000000 |> Random.map TargetPortfolioSize )
        ]


investmentSizeGen : Generator Investment.Size
investmentSizeGen =
    investment0to5kRange


investmentShareGen : Generator InvestmentShare
investmentShareGen =
    Random.frequency
        [ ( 1, Random.constant InvestmentShare.NotSpecified )
        , ( 2, Random.map InvestmentSharePercent percentageGen )
        ]


targetBalanceGen : Generator TargetBalance
targetBalanceGen =
    Random.frequency
        [ ( 1, Random.constant TargetBalance.NotSpecified )
        , ( 2, Random.int 200 100000 |> Random.map TargetBalance )
        ]


confirmationSettingsGen : Generator ConfirmationSettings
confirmationSettingsGen =
    nonemptySubset Rating.allRatings |> Random.map RatingList


nonemptySubset : List a -> Generator (List a)
nonemptySubset =
    subset 1


subset : Int -> List a -> Generator (List a)
subset minimumElements whatToSamleFrom =
    let
        totalElemCount =
            List.length whatToSamleFrom
    in
    Random.int minimumElements totalElemCount
        |> Random.map (\selectedElemCount -> List.repeat selectedElemCount True ++ List.repeat (totalElemCount - selectedElemCount) False)
        |> Random.andThen (\bools -> Random.List.shuffle bools)
        |> Random.map
            (\bools ->
                List.map2 (,) bools whatToSamleFrom
                    |> List.filterMap
                        (\( flag, thing ) ->
                            if flag then
                                Just thing
                            else
                                Nothing
                        )
            )


percentageGen : Generator Int
percentageGen =
    Random.int 0 100


{-| Generate random range (from, to) such that 0 <= from <= to <= 100
-}
percentRangeGen : Generator ( Int, Int )
percentRangeGen =
    percentageGen
        |> Random.andThen (\from -> percentageFrom from)


percentageFrom : Int -> Generator ( Int, Int )
percentageFrom from =
    Random.int from 100 |> Random.map (\to -> ( from, to ))


{-| To generate valid portfolio structure we need 8 non-negative ints that add up to 100.
Start with [100], split all numbers randomly into two and then repeat twice to get list of 8 ints.
-}
eightIntsThatAddUpTo100 : Generator (List Int)
eightIntsThatAddUpTo100 =
    let
        step : List Int -> Generator (List Int)
        step =
            List.map subdivide >> Random.combine >> Random.map List.concat
    in
    step [ 100 ]
        |> Random.andThen (\twoInts -> step twoInts)
        |> Random.andThen (\fourInts -> step fourInts)


subdivide : Int -> Generator (List Int)
subdivide numToSplit =
    Random.int 0 numToSplit |> Random.map (\split -> [ split, numToSplit - split ])
