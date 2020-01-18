module Data.Migration.Strategy.V5 exposing
    ( StrategyConfiguration
    , fromV4
    , strategyDecoder
    )

import Data.Filter as Filters exposing (BuyingConfiguration, SellingConfiguration)
import Data.Filter.Conditions.Rating as Rating
import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V1.Investment as Investment exposing (InvestmentsPerRating)
import Data.Migration.Strategy.V4 as V4
import Data.Migration.Strategy.V4.PortfolioStructure as V4PS
import Data.Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as V5PS exposing (PortfolioStructure)
import Json.Decode as Decode exposing (Decoder)
import Percentage


{-| V4 -> V5: V5 changes PortfolioShares - they're no longer defined in terms of ranges (min - max).
Instead each rating has just one associated value (corresponding to what used to be "max")
-}
fromV4 : V4.StrategyConfiguration -> ( StrategyConfiguration, List MigrationWarning )
fromV4 old =
    let
        ( newPortfolioStructure, warnings ) =
            case old.generalSettings.portfolio of
                Conservative ->
                    ( V5PS.conservative, [] )

                Balanced ->
                    ( V5PS.balanced, [] )

                Progressive ->
                    ( V5PS.progressive, [] )

                UserDefined ->
                    migratePortfolioStructure old.portfolioShares
    in
    ( { generalSettings = old.generalSettings
      , portfolioStructure = newPortfolioStructure
      , investmentSizeOverrides = old.investmentSizeOverrides
      , buyingConfig = old.buyingConfig
      , sellingConfig = old.sellingConfig
      }
    , warnings
    )


type alias StrategyConfiguration =
    { generalSettings : V4.GeneralSettings
    , portfolioStructure : PortfolioStructure
    , investmentSizeOverrides : InvestmentsPerRating
    , buyingConfig : BuyingConfiguration
    , sellingConfig : SellingConfiguration
    }


migratePortfolioStructure : V4PS.PortfolioShares -> ( V5PS.PortfolioStructure, List MigrationWarning )
migratePortfolioStructure v4shares =
    let
        sharesDefinedWithRanges =
            v4shares
                |> Rating.ratingDictToList
                |> List.map (\( rating, slider ) -> ( rating, V4PS.toIntRange slider ))
                -- Warning will be shown if user had minimum /= maximum for any rating
                |> List.filter (\( _, ( min, max ) ) -> min /= max)

        simplificationWarning =
            if List.isEmpty sharesDefinedWithRanges then
                []

            else
                sharesDefinedWithRanges
                    |> List.map
                        (\( rating, ( min, max ) ) ->
                            "\u{00A0}\u{00A0}Požadovaný podíl investovaný do půjček s úročením "
                                ++ Rating.showInterestPercent rating
                                ++ " byl změněn z rozsahu '"
                                ++ String.fromInt min
                                ++ " až "
                                ++ String.fromInt max
                                ++ "%' na '"
                                ++ String.fromInt max
                                ++ "%'"
                        )
                    |> (\items -> "strategie měla nastavenu vámi definovanou strukturu portfolia, která musela být zjednodušena:" :: items)

        maxima =
            Rating.ratingDictToList v4shares
                |> List.map (Percentage.fromInt << Tuple.second << V4PS.toIntRange << Tuple.second)
    in
    case V5PS.fromPercentageList maxima of
        Ok v5PortfolioStructure ->
            ( v5PortfolioStructure, simplificationWarning )

        Err errorAboutIncorrectStructure ->
            -- This shouldn't happen, unless people manually meddle with the URL encoded strategy
            ( V5PS.conservative
            , [ errorAboutIncorrectStructure
                    ++ " \nNastavil jsem 'konzervativní' portfolio, které si můžete v sekci 'Struktura portfolia' upravit podle potřeby."
              ]
            )


strategyDecoder : Decoder StrategyConfiguration
strategyDecoder =
    {- Need the portfolio ahead of time because it determines if we
       should decode portfolio structure (for UserDefined) or use one of the predefined ones
    -}
    Decode.field "h" V4.generalSettingsDecoder
        |> Decode.andThen
            (\generalSettings ->
                Decode.map4 (StrategyConfiguration generalSettings)
                    (V5PS.decoderFromPortfolio generalSettings.portfolio)
                    (Decode.field "j" Investment.decoder)
                    (Decode.field "k" Filters.decodeBuyingConfiguration)
                    (Decode.field "l" Filters.decodeSellingConfiguration)
            )
