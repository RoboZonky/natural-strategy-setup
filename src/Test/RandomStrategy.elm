module Test.RandomStrategy exposing (strategyConfigurationGen)

import AllDict exposing (AllDict)
import Data.Confirmation exposing (ConfirmationSettings)
import Data.Filter exposing (FilteredItem(..), MarketplaceFilter(..))
import Data.Filter.Conditions exposing (Conditions)
import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition(AmountCondition))
import Data.Filter.Conditions.Interest as Interest exposing (Interest, InterestCondition(..))
import Data.Filter.Conditions.LoanPurpose as LoanPurpose exposing (LoanPurposeCondition(LoanPurposeList))
import Data.Filter.Conditions.LoanTerm as LoanTerm exposing (LoanTermCondition(..))
import Data.Filter.Conditions.MainIncome as MainIncome exposing (MainIncomeCondition(..))
import Data.Filter.Conditions.Rating as Rating exposing (Rating, RatingCondition(RatingList))
import Data.Filter.Conditions.Region as Region exposing (RegionCondition(RegionList))
import Data.Filter.Conditions.Story exposing (Story(..), StoryCondition(StoryCondition))
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare(..))
import Data.Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure exposing (PortfolioShares)
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration)
import Data.TargetBalance as TargetBalance exposing (TargetBalance(..))
import Data.TargetPortfolioSize as TargetPortfoliSize exposing (TargetPortfolioSize(TargetPortfolioSize))
import Random exposing (Generator)
import Random.Extra as Random


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
                List.map2 (\rtg share -> ( rtg, share )) Rating.allRatings sharesList
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
    Random.int 0 25 |> Random.andThen (\from -> Random.int from 25 |> Random.map (\to -> ( 200 * from, 200 * to )))


buyFiltersGen : Generator (List MarketplaceFilter)
buyFiltersGen =
    Random.rangeLengthList 0 10 marketplaceFilterGen


sellFiltersGen : Generator (List MarketplaceFilter)
sellFiltersGen =
    -- TODO generate list of SecondaryMarketplaceFilter
    Random.constant []


marketplaceFilterGen : Generator MarketplaceFilter
marketplaceFilterGen =
    filteredItemGen
        |> Random.andThen
            (\item ->
                case item of
                    Loan ->
                        Random.map2 (\pos neg -> MarketplaceFilter { whatToFilter = Loan, ignoreWhen = pos, butNotWhen = neg }) loanConditions loanConditions

                    other ->
                        Random.map2 (\pos neg -> MarketplaceFilter { whatToFilter = other, ignoreWhen = pos, butNotWhen = neg }) participationConditions participationConditions
            )


loanConditions : Generator Conditions
loanConditions =
    conditionsPartSharedByAllFilteredItems
        |> Random.andMap (maybeCondition amountConditionGen)


participationConditions : Generator Conditions
participationConditions =
    conditionsPartSharedByAllFilteredItems
        |> Random.andMap (Random.constant Nothing)


conditionsPartSharedByAllFilteredItems : Generator (Maybe AmountCondition -> Conditions)
conditionsPartSharedByAllFilteredItems =
    Random.map Conditions (maybeCondition regionConditionGen)
        |> Random.andMap (maybeCondition ratingConditionGen)
        |> Random.andMap (maybeCondition incomeConditionGen)
        |> Random.andMap (maybeCondition purposeConditionGen)
        |> Random.andMap (maybeCondition storyConditionGen)
        |> Random.andMap (maybeCondition termConditionGen)
        |> Random.andMap (maybeCondition interestConditionGen)


maybeCondition : Generator cond -> Generator (Maybe cond)
maybeCondition cgen =
    Random.frequency [ ( 1, Random.constant Nothing ), ( 1, Random.map Just cgen ) ]


regionConditionGen : Generator RegionCondition
regionConditionGen =
    randomSubset Region.allRegions |> Random.map RegionList


ratingConditionGen : Generator RatingCondition
ratingConditionGen =
    randomSubset Rating.allRatings |> Random.map RatingList


incomeConditionGen : Generator MainIncomeCondition
incomeConditionGen =
    randomSubset MainIncome.allIncomes |> Random.map MainIncomeList


purposeConditionGen : Generator LoanPurposeCondition
purposeConditionGen =
    randomSubset LoanPurpose.allPurposes |> Random.map LoanPurposeList


storyConditionGen : Generator StoryCondition
storyConditionGen =
    Random.sample [ SHORT, BELOW_AVERAGE, AVERAGE, ABOVE_AVERAGE ]
        |> Random.map (Maybe.withDefault SHORT >> StoryCondition)


termConditionGen : Generator LoanTermCondition
termConditionGen =
    let
        maxLoanTerm =
            84
    in
    Random.choices
        [ Random.map LoanTerm.LessThan (Random.int 0 maxLoanTerm)
        , Random.int 0 maxLoanTerm |> Random.andThen (\mi -> Random.int mi maxLoanTerm |> Random.map (\mx -> LoanTerm.Between mi mx))
        , Random.map LoanTerm.MoreThan (Random.int 0 maxLoanTerm)
        ]
        |> Random.map LoanTermCondition


amountConditionGen : Generator AmountCondition
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
        |> Random.map AmountCondition


interestConditionGen : Generator InterestCondition
interestConditionGen =
    Random.choices
        [ Random.map Interest.LessThan (Random.float 0 100)
        , Random.float 0 100 |> Random.andThen (\mi -> Random.float mi 100 |> Random.map (\mx -> Interest.Between mi mx))
        , Random.map Interest.MoreThan (Random.float 0 100)
        ]
        |> Random.map InterestCondition


filteredItemGen : Generator FilteredItem
filteredItemGen =
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
    randomSubset Rating.allRatings |> Random.map RatingList


randomSubset : List a -> Generator (List a)
randomSubset whatToSamleFrom =
    Random.list (List.length whatToSamleFrom) Random.bool
        |> Random.map
            (\bools ->
                List.map2 (,) bools whatToSamleFrom
                    |> List.filterMap
                        (\( flag, thing ) ->
                            if flag then
                                Nothing
                            else
                                Just thing
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
