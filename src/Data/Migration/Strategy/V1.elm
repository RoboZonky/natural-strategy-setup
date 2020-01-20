module Data.Migration.Strategy.V1 exposing
    ( DecodedStrategy
    , GeneralSettings
    , StrategyConfiguration
    , strategyDecoder
    )

import Data.ExitConfig as ExitConfig exposing (ExitConfig)
import Data.Filter exposing (BuyingConfiguration(..), MarketplaceEnablement, MarketplaceFilter, SellingConfiguration(..), filteredItemDecoder)
import Data.Filter.Conditions exposing (Conditions)
import Data.Filter.Conditions.Amount as Amount
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent
import Data.Filter.Conditions.Income as Income
import Data.Filter.Conditions.Insurance as Insurance
import Data.Filter.Conditions.Purpose as Purpose
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..), RatingCondition(..))
import Data.Filter.Conditions.Region as Region
import Data.Filter.Conditions.RemainingAmount as RemainingAmount
import Data.Filter.Conditions.RemainingTermMonths as RemainingTermMonths
import Data.Filter.Conditions.Story as Story
import Data.Filter.Conditions.TermPercent as TermPercent
import Data.Migration.Strategy.V1.Investment as Investment exposing (InvestmentsPerRating)
import Data.Migration.Strategy.V1.TargetBalance as TargetBalance exposing (TargetBalance)
import Data.Migration.Strategy.V4.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.Migration.Strategy.V5.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing (andMap, optionalField)


type alias StrategyConfiguration =
    { generalSettings : GeneralSettings
    , portfolioShares : PortfolioShares
    , investmentSizeOverrides : InvestmentsPerRating
    , buyingConfig : BuyingConfiguration
    , sellingConfig : SellingConfiguration
    }


type alias GeneralSettings =
    { portfolio : Portfolio
    , exitConfig : ExitConfig
    , targetPortfolioSize : TargetPortfolioSize
    , defaultInvestmentSize : Investment.Size
    , defaultInvestmentShare : InvestmentShare
    , defaultTargetBalance : TargetBalance
    , confirmationSettings : RatingCondition
    }


type alias DecodedStrategy =
    { strategyConfig : StrategyConfiguration
    , removedBuyFilterCount : Int
    , removedSellFilterCount : Int
    }



-- JSON


strategyDecoder : Decoder DecodedStrategy
strategyDecoder =
    Decode.field "k" decodeBuyingConfiguration
        |> Decode.andThen
            (\( buyingConfig, removedBuyFilterCount ) ->
                Decode.field "l" decodeSellingConfiguration
                    |> Decode.andThen
                        (\( sellingConfig, removedSellFilterCount ) ->
                            Decode.map3 StrategyConfiguration
                                (Decode.field "h" generalSettingsDecoder)
                                (Decode.field "i" PortfolioStructure.decoder)
                                (Decode.field "j" Investment.decoder)
                                |> Decode.map
                                    (\sc ->
                                        { strategyConfig = sc buyingConfig sellingConfig
                                        , removedBuyFilterCount = removedBuyFilterCount
                                        , removedSellFilterCount = removedSellFilterCount
                                        }
                                    )
                        )
            )


generalSettingsDecoder : Decoder GeneralSettings
generalSettingsDecoder =
    Decode.map7 GeneralSettings
        (Decode.field "a" Portfolio.decoder)
        (Decode.field "b" ExitConfig.decoder)
        (Decode.field "c" TargetPortfolioSize.decoder)
        (Decode.field "d" Investment.sizeDecoder)
        (Decode.field "e" InvestmentShare.decoder)
        (Decode.field "f" TargetBalance.decoder)
        (Decode.field "g" Rating.conditionDecoder)


decodeBuyingConfiguration : Decoder ( BuyingConfiguration, Int )
decodeBuyingConfiguration =
    Decode.field "o" Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    0 ->
                        Decode.succeed ( InvestEverything, 0 )

                    1 ->
                        Decode.field "r" (Decode.list maybeMarketplaceFilterDecoder)
                            |> Decode.map partitionMaybeFilters
                            |> Decode.andThen
                                (\( filters, invalidFilterCount ) ->
                                    decodeMarketplaceEnablement
                                        |> Decode.andThen
                                            (\marketplaceEnablement ->
                                                Decode.succeed
                                                    ( InvestSomething marketplaceEnablement filters
                                                    , invalidFilterCount
                                                    )
                                            )
                                )

                    2 ->
                        Decode.succeed ( InvestNothing, 0 )

                    _ ->
                        Decode.fail <| "Unable to decode BuyingConfiguration from " ++ String.fromInt x
            )


decodeMarketplaceEnablement : Decoder MarketplaceEnablement
decodeMarketplaceEnablement =
    Decode.map2 MarketplaceEnablement
        (Decode.field "p" Decode.bool)
        (Decode.field "q" Decode.bool)


{-| Decode selling config together with the number of filters that had to be removed,
because they used legacy conditions
-}
decodeSellingConfiguration : Decoder ( SellingConfiguration, Int )
decodeSellingConfiguration =
    Decode.field "m" Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    0 ->
                        Decode.succeed ( SellNothing, 0 )

                    1 ->
                        Decode.field "n" (Decode.list maybeMarketplaceFilterDecoder)
                            |> Decode.map partitionMaybeFilters
                            |> Decode.andThen
                                (\( filters, invalidFilterCount ) ->
                                    if List.isEmpty filters then
                                        Decode.succeed ( SellNothing, invalidFilterCount )

                                    else
                                        Decode.succeed ( SellSomething filters, invalidFilterCount )
                                )

                    _ ->
                        Decode.fail <| "Unable to decode SellingConfiguration from " ++ String.fromInt x
            )


partitionMaybeFilters : List (Maybe MarketplaceFilter) -> ( List MarketplaceFilter, Int )
partitionMaybeFilters =
    List.foldl
        (\mFilter ( fs, invalidCount ) ->
            case mFilter of
                Nothing ->
                    ( fs, invalidCount + 1 )

                Just f ->
                    ( fs ++ [ f ], invalidCount )
        )
        ( [], 0 )


{-| Decode filter as Nothing if it contained any of the legacy conditions
-}
maybeMarketplaceFilterDecoder : Decoder (Maybe MarketplaceFilter)
maybeMarketplaceFilterDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "t" conditionsDecoder)
        (Decode.field "u" conditionsDecoder)
        |> Decode.andThen
            (\( mIgnoreWhen, mButNotWhen ) ->
                case ( mIgnoreWhen, mButNotWhen ) of
                    ( Just ignoreWhen, Just butNotWhen ) ->
                        Decode.map
                            (\filteredItem -> Just <| MarketplaceFilter filteredItem ignoreWhen butNotWhen)
                            (Decode.field "s" filteredItemDecoder)

                    _ ->
                        Decode.succeed Nothing
            )


{-| Decode conditions as nothing if it contained Rating or Interest condition
-}
conditionsDecoder : Decoder (Maybe Conditions)
conditionsDecoder =
    Json.Decode.Extra.keys
        |> Decode.andThen
            (\keys ->
                let
                    containsLegacyConditions =
                        List.member "B" keys || List.member "J" keys
                in
                if containsLegacyConditions then
                    Decode.succeed Nothing

                else
                    Decode.succeed Conditions
                        |> andMap (optionalField "A" Region.conditionDecoder)
                        -- Rating condition was removed, so just ignore it
                        -- |> andMap (optionalField "B" Rating.conditionDecoder)
                        |> andMap (optionalField "C" Income.conditionDecoder)
                        |> andMap (optionalField "D" Purpose.conditionDecoder)
                        |> andMap (optionalField "E" Story.conditionDecoder)
                        |> andMap (optionalField "F" RemainingTermMonths.conditionDecoder)
                        |> andMap (optionalField "G" TermPercent.conditionDecoder)
                        |> andMap (optionalField "H" ElapsedTermMonths.conditionDecoder)
                        |> andMap (optionalField "I" ElapsedTermPercent.conditionDecoder)
                        -- Interest condition in V1 were based on arbitrary float percentages -> remove them
                        |> andMap (Decode.succeed Nothing)
                        |> andMap (optionalField "K" Amount.conditionDecoder)
                        |> andMap (optionalField "L" Insurance.conditionDecoder)
                        |> andMap (optionalField "M" RemainingAmount.conditionDecoder)
                        -- Loan Annuity condition was added in V2
                        |> andMap (Decode.succeed Nothing)
                        -- Revenue Rate condition was added in V2
                        |> andMap (Decode.succeed Nothing)
                        -- Sale Fee condition was added in RoboZonky 5.2.0
                        |> andMap (Decode.succeed Nothing)
                        -- Relative Profit condition added in RoboZonky 5.7.0
                        |> andMap (Decode.succeed Nothing)
                        |> Decode.map Just
            )
