module Test.RandomStrategy exposing (conditionsGen, strategyConfigurationGen)

import Array
import Data.Confirmation as Confirmation exposing (ConfirmationSettings)
import Data.ExitConfig as ExitConfig
import Data.Filter as Filter exposing (BuyingConfiguration, FilteredItem(..), MarketplaceEnablement, MarketplaceFilter, SellingConfiguration)
import Data.Filter.Conditions exposing (Condition(..), Conditions, addCondition, emptyConditions)
import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition(..))
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonthsCondition(..))
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercentCondition(..))
import Data.Filter.Conditions.Income as Income exposing (IncomeCondition(..))
import Data.Filter.Conditions.Insurance exposing (Insurance(..), InsuranceCondition(..))
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition(..))
import Data.Filter.Conditions.LoanAnnuity as LoanAnnuity exposing (LoanAnnuityCondition(..))
import Data.Filter.Conditions.Purpose as Purpose exposing (PurposeCondition(..))
import Data.Filter.Conditions.Rating as Rating exposing (Rating, RatingCondition(..))
import Data.Filter.Conditions.Region as Region exposing (RegionCondition(..))
import Data.Filter.Conditions.RemainingAmount as RemainingAmount exposing (RemainingAmountCondition(..))
import Data.Filter.Conditions.RevenueRate as RevenueRate exposing (RevenueRateCondition(..))
import Data.Filter.Conditions.Story exposing (Story(..), StoryCondition(..))
import Data.Filter.Conditions.TermMonths as TermMonths exposing (TermMonthsCondition(..))
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercentCondition(..))
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.ReservationSetting exposing (ReservationSetting(..))
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration)
import Data.TargetBalance as TargetBalance exposing (TargetBalance(..))
import Data.TargetPortfolioSize as TargetPortfoliSize exposing (TargetPortfolioSize(..))
import Random exposing (Generator)
import Random.Extra as Random
import Random.List


strategyConfigurationGen : Generator StrategyConfiguration
strategyConfigurationGen =
    Random.map5 StrategyConfiguration
        generalSettingsGen
        portfolioSharesGen
        investmentsPerRatingGen
        buyingConfigGen
        sellingConfigGen


generalSettingsGen : Generator GeneralSettings
generalSettingsGen =
    Random.map GeneralSettings portfolioGen
        |> Random.andMap exitConfigGen
        |> Random.andMap targetPortfolioSizeGen
        |> Random.andMap investmentSizeGen
        |> Random.andMap investmentShareGen
        |> Random.andMap targetBalanceGen
        |> Random.andMap confirmationSettingsGen
        |> Random.andMap reservationSettingGen


reservationSettingGen : Generator ReservationSetting
reservationSettingGen =
    Random.choices (Random.constant AcceptMatching)
        [ -- Random.constant FullOwnership
          Random.constant Ignore
        ]


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
                    |> Rating.initRatingDict
            )


investmentsPerRatingGen : Generator InvestmentsPerRating
investmentsPerRatingGen =
    Random.list (List.length Rating.allRatings) investment0to5kRange
        |> Random.map
            (\investmentSizes ->
                List.map2 (\rtg sz -> ( rtg, sz )) Rating.allRatings investmentSizes
                    |> Rating.initRatingDict
            )


investment0to5kRange : Generator Investment.Size
investment0to5kRange =
    randomRangeGen 0 25
        |> Random.map (\( from, to ) -> Investment.mkSize (200 * from) (200 * to))


buyingConfigGen : Generator BuyingConfiguration
buyingConfigGen =
    Random.frequency ( 1, Random.constant Filter.InvestNothing )
        [ ( 2, Random.map2 Filter.InvestSomething marketplaceEnablementGen (Random.rangeLengthList 0 10 buyFilterGen) )
        , ( 1, Random.constant Filter.InvestEverything )
        ]


marketplaceEnablementGen : Generator MarketplaceEnablement
marketplaceEnablementGen =
    Random.map2 MarketplaceEnablement Random.bool Random.bool


sellingConfigGen : Generator SellingConfiguration
sellingConfigGen =
    Random.frequency ( 1, Random.constant Filter.SellNothing )
        [ -- Generate nonempty filter list here, as empty filter list is invalid, prevented by form validation
          ( 3, Random.map Filter.SellSomething (Random.rangeLengthList 1 10 sellFilterGen) )
        ]


buyFilterGen : Generator MarketplaceFilter
buyFilterGen =
    filteredItemGen
        |> Random.andThen filterGen


sellFilterGen : Generator MarketplaceFilter
sellFilterGen =
    filterGen Participation_To_Sell


filterGen : FilteredItem -> Generator MarketplaceFilter
filterGen filteredItem =
    Random.map2 (\pos neg -> { whatToFilter = filteredItem, ignoreWhen = pos, butNotWhen = neg })
        (conditionsGen 1 filteredItem)
        (conditionsGen 0 filteredItem)


conditionsGen : Int -> FilteredItem -> Generator Conditions
conditionsGen minConditions filteredItem =
    let
        extraConditions =
            case filteredItem of
                Participation ->
                    participationSpecificConditions

                Participation_To_Sell ->
                    participationSpecificConditions

                Loan_And_Participation ->
                    []

                Loan ->
                    []
    in
    conditionSubsetGen minConditions (conditionsSharedByAllFilteredItems ++ extraConditions)


conditionSubsetGen : Int -> List (Generator Condition) -> Generator Conditions
conditionSubsetGen minimumConditions conditionsToPick =
    subset minimumConditions conditionsToPick
        |> Random.andThen (Random.combine >> Random.map (List.foldl addCondition emptyConditions))


conditionsSharedByAllFilteredItems : List (Generator Condition)
conditionsSharedByAllFilteredItems =
    [ Random.map Condition_Amount amountConditionGen
    , Random.map Condition_Region regionConditionGen
    , Random.map Condition_Income incomeConditionGen
    , Random.map Condition_Purpose purposeConditionGen
    , Random.map Condition_Story storyConditionGen
    , Random.map Condition_Term_Months termMonthsConditionGen
    , Random.map Condition_Interest interestConditionGen
    , Random.map Condition_Insurance insuranceConditionGen
    , Random.map Condition_Loan_Annuity loanAnnuityConditionGen
    , Random.map Condition_Revenue_Rate revenueRateConditionGen
    ]


participationSpecificConditions : List (Generator Condition)
participationSpecificConditions =
    [ Random.map Condition_Term_Percent termPercentConditionGen
    , Random.map Condition_Elapsed_Term_Months elapsedTermMonthsConditionGen
    , Random.map Condition_Elapsed_Term_Percent elapsedTermPercentConditionGen
    , Random.map Condition_Remaining_Amount remainingAmountConditionGen
    ]


regionConditionGen : Generator RegionCondition
regionConditionGen =
    nonemptySubset Region.allRegions |> Random.map RegionList


incomeConditionGen : Generator IncomeCondition
incomeConditionGen =
    nonemptySubset Income.allIncomes |> Random.map IncomeList


purposeConditionGen : Generator PurposeCondition
purposeConditionGen =
    nonemptySubset Purpose.allPurposes |> Random.map PurposeList


storyConditionGen : Generator StoryCondition
storyConditionGen =
    Random.sample [ SHORT, BELOW_AVERAGE, AVERAGE, ABOVE_AVERAGE ]
        |> Random.map (Maybe.withDefault SHORT >> StoryCondition)


insuranceConditionGen : Generator InsuranceCondition
insuranceConditionGen =
    Random.sample [ Active, Inactive ]
        |> Random.map (Maybe.withDefault Active >> InsuranceCondition)


termMonthsConditionGen : Generator TermMonthsCondition
termMonthsConditionGen =
    let
        minTermMonths =
            0

        maxTermMonths =
            84
    in
    Random.choices (Random.map TermMonths.LessThan (Random.int (minTermMonths + 1 {- 0 is invalid, as parser subtract 1 -}) (maxTermMonths + 1)))
        [ randomRangeGen minTermMonths maxTermMonths
            |> Random.map (\( mi, mx ) -> TermMonths.Between mi mx)
        , Random.map TermMonths.MoreThan (Random.int minTermMonths (maxTermMonths - 1 {- max is invalid, as parser adds 1 -}))
        ]
        |> Random.map TermMonthsCondition


loanAnnuityConditionGen : Generator LoanAnnuityCondition
loanAnnuityConditionGen =
    let
        maxAmount =
            1000000
    in
    Random.choices (Random.map LoanAnnuity.LessThan (Random.int 0 maxAmount))
        [ randomRangeGen 0 maxAmount |> Random.map (\( mi, mx ) -> LoanAnnuity.Between mi mx)
        , Random.map LoanAnnuity.MoreThan (Random.int 0 maxAmount)
        ]
        |> Random.map LoanAnnuityCondition


revenueRateConditionGen : Generator RevenueRateCondition
revenueRateConditionGen =
    let
        maxAmount =
            200
    in
    Random.choices (Random.map RevenueRate.LessThan (Random.float 0 maxAmount))
        [ randomFloatRangeGen 0 maxAmount |> Random.map (\( mi, mx ) -> RevenueRate.Between mi mx)
        , Random.map RevenueRate.MoreThan (Random.float 0 maxAmount)
        ]
        |> Random.map RevenueRateCondition


termPercentConditionGen : Generator TermPercentCondition
termPercentConditionGen =
    let
        minTermPercent =
            0

        maxTermPercent =
            100
    in
    Random.choices (Random.map TermPercent.LessThan (Random.int (minTermPercent + 1 {- 0 is invalid, as parser subtract 1 -}) (maxTermPercent + 1)))
        [ percentRangeGen |> Random.map (\( mi, mx ) -> TermPercent.Between mi mx)
        , Random.map TermPercent.MoreThan (Random.int minTermPercent (maxTermPercent - 1 {- max is invalid, as parser adds 1 -}))
        ]
        |> Random.map TermPercentCondition


elapsedTermMonthsConditionGen : Generator ElapsedTermMonthsCondition
elapsedTermMonthsConditionGen =
    let
        minTermMonths =
            0

        maxTermMonths =
            84
    in
    Random.choices (Random.map ElapsedTermMonths.LessThan (Random.int (minTermMonths + 1 {- 0 is invalid, as parser subtract 1 -}) (maxTermMonths + 1)))
        [ randomRangeGen minTermMonths maxTermMonths
            |> Random.map (\( mi, mx ) -> ElapsedTermMonths.Between mi mx)
        , Random.map ElapsedTermMonths.MoreThan (Random.int minTermMonths (maxTermMonths - 1 {- max is invalid, as parser adds 1 -}))
        ]
        |> Random.map ElapsedTermMonthsCondition


elapsedTermPercentConditionGen : Generator ElapsedTermPercentCondition
elapsedTermPercentConditionGen =
    let
        minTermPercent =
            0

        maxTermPercent =
            100
    in
    Random.choices (Random.map ElapsedTermPercent.LessThan (Random.int (minTermPercent + 1 {- 0 is invalid, as parser subtract 1 -}) (maxTermPercent + 1)))
        [ percentRangeGen |> Random.map (\( mi, mx ) -> ElapsedTermPercent.Between mi mx)
        , Random.map ElapsedTermPercent.MoreThan (Random.int minTermPercent (maxTermPercent - 1 {- max is invalid, as parser adds 1 -}))
        ]
        |> Random.map ElapsedTermPercentCondition


amountConditionGen : Generator AmountCondition
amountConditionGen =
    let
        maxAmount =
            1000000
    in
    Random.choices (Random.map Amount.LessThan (Random.int 0 maxAmount))
        [ randomRangeGen 0 maxAmount |> Random.map (\( mi, mx ) -> Amount.Between mi mx)
        , Random.map Amount.MoreThan (Random.int 0 maxAmount)
        ]
        |> Random.map AmountCondition


remainingAmountConditionGen : Generator RemainingAmountCondition
remainingAmountConditionGen =
    let
        maxRemainingAmount =
            1000000
    in
    Random.choices (Random.map RemainingAmount.LessThan (Random.int 0 maxRemainingAmount))
        [ randomRangeGen 0 maxRemainingAmount |> Random.map (\( mi, mx ) -> RemainingAmount.Between mi mx)
        , Random.map RemainingAmount.MoreThan (Random.int 0 maxRemainingAmount)
        ]
        |> Random.map RemainingAmountCondition


interestConditionGen : Generator InterestCondition
interestConditionGen =
    Random.choices (Random.map Interest.LessThan ratingGen)
        [ Random.map (\( r1, r2 ) -> Interest.Between r1 r2) ratingRangeGen
        , Random.map Interest.MoreThan ratingGen
        , Random.map Interest.Exactly ratingGen
        ]
        |> Random.map InterestCondition


ratingGen : Generator Rating
ratingGen =
    Random.sample Rating.allRatings |> Random.map (Maybe.withDefault Rating.D)


{-| pair of ratings of which the 1st is better than the 2nd
-}
ratingRangeGen : Generator ( Rating, Rating )
ratingRangeGen =
    let
        rtgs =
            Array.fromList Rating.allRatings
    in
    randomRangeGtGen 0 (List.length Rating.allRatings - 1)
        |> Random.map
            (\( i, j ) ->
                Maybe.map2 Tuple.pair (Array.get i rtgs) (Array.get j rtgs)
                    |> Maybe.withDefault ( Rating.D, Rating.D )
            )


filteredItemGen : Generator FilteredItem
filteredItemGen =
    {- Intentionally NOT generating Participation_To_Sell - we're only generating items for BUY filters -}
    Random.sample [ Loan, Participation, Loan_And_Participation ]
        |> Random.map (Maybe.withDefault Loan)


portfolioGen : Generator Portfolio
portfolioGen =
    Random.sample [ Conservative, Balanced, Progressive, UserDefined ]
        |> Random.map (Maybe.withDefault Conservative)


targetPortfolioSizeGen : Generator TargetPortfolioSize
targetPortfolioSizeGen =
    Random.frequency ( 1, Random.constant TargetPortfoliSize.NotSpecified )
        [ ( 2, Random.int 0 1000000 |> Random.map TargetPortfolioSize ) ]


investmentSizeGen : Generator Investment.Size
investmentSizeGen =
    investment0to5kRange


investmentShareGen : Generator InvestmentShare
investmentShareGen =
    Random.frequency ( 1, Random.constant InvestmentShare.NotSpecified )
        [ ( 2, Random.map InvestmentShare.Percent <| Random.int 1 100 ) ]


targetBalanceGen : Generator TargetBalance
targetBalanceGen =
    Random.frequency ( 1, Random.constant TargetBalance.NotSpecified )
        [ ( 2, Random.int 200 100000 |> Random.map TargetBalance ) ]


confirmationSettingsGen : Generator ConfirmationSettings
confirmationSettingsGen =
    Random.frequency ( 1, Random.constant Confirmation.NoConfirmation )
        [ ( 1, Random.map Confirmation.Confirm interestConditionGen ) ]


exitConfigGen : Generator ExitConfig.ExitConfig
exitConfigGen =
    let
        exitByWithSelloffGen =
            {- Not really random, just bumping day by 1 to make sellof date 1 day earlier -}
            Random.map (\( d, m, y ) -> ExitConfig.ExitByWithSelloff (dateToString ( d + 1, m, y )) (dateToString ( d, m, y ))) dateGen
    in
    Random.frequency ( 1, Random.constant ExitConfig.DontExit )
        [ ( 2, Random.map (ExitConfig.ExitBy << dateToString) dateGen )
        , ( 3, exitByWithSelloffGen )
        ]


dateToString : ( Int, Int, Int ) -> String
dateToString ( day, month, year ) =
    String.join "." <| List.map String.fromInt [ day, month, year ]


dateGen : Generator ( Int, Int, Int )
dateGen =
    Random.map3 (\a b c -> ( a, b, c ))
        (Random.int 1 27)
        (Random.int 1 12)
        (Random.int 1970 2100)


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
                List.map2 (\a b -> ( a, b )) bools whatToSamleFrom
                    |> List.filterMap
                        (\( flag, thing ) ->
                            if flag then
                                Just thing

                            else
                                Nothing
                        )
            )


{-| Generate random range (from, to) such that 0 <= from <= to <= 100
-}
percentRangeGen : Generator ( Int, Int )
percentRangeGen =
    randomRangeGen 0 100


percentageFrom : Int -> Generator ( Int, Int )
percentageFrom from =
    Random.int from 100 |> Random.map (\to -> ( from, to ))


{-| Generate pair (x, y) such that mi <= x <= y <= ma
-}
randomRangeGen : Int -> Int -> Generator ( Int, Int )
randomRangeGen mi ma =
    Random.int mi ma
        |> Random.andThen
            (\generatedMin ->
                Random.int generatedMin ma
                    |> Random.map (\generatedMax -> ( generatedMin, generatedMax ))
            )


randomFloatRangeGen : Float -> Float -> Generator ( Float, Float )
randomFloatRangeGen mi ma =
    Random.float mi ma
        |> Random.andThen
            (\generatedMin ->
                Random.float generatedMin ma
                    |> Random.map (\generatedMax -> ( generatedMin, generatedMax ))
            )


{-| Generate pair (x, y) such that mi <= x < y <= ma
I.e. unlike randomRangeGen the 2nd number will always be greater than the first
-}
randomRangeGtGen : Int -> Int -> Generator ( Int, Int )
randomRangeGtGen mi ma =
    Random.int mi (ma - 1)
        |> Random.andThen
            (\generatedMin ->
                Random.int (generatedMin + 1) ma
                    |> Random.map (\generatedMax -> ( generatedMin, generatedMax ))
            )


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
