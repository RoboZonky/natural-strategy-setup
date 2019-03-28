module Data.VersionedStrategy exposing (loadStrategy)

import Base64
import Data.Confirmation as Confirmation
import Data.Filter.Conditions.Rating as Rating
import Data.Migration.StrategyV1 as V1
import Data.ReservationSetting as ReservationSetting
import Data.Strategy as V2
import Json.Decode as Decode
import Types exposing (UrlHash)


type VersionedStrategy
    = V1 V1.DecodedStrategy
    | V2 V2.StrategyConfiguration


loadStrategy : UrlHash -> Result String ( V2.StrategyConfiguration, List String )
loadStrategy hash =
    case versionedStrategyFromUrlHash hash of
        Ok (V1 decodedStrategy) ->
            Ok (migrate_v1_to_v2 decodedStrategy)

        Ok (V2 stategyCofig) ->
            Ok ( stategyCofig, [] )

        Err e ->
            Err e


migrate_v1_to_v2 : V1.DecodedStrategy -> ( V2.StrategyConfiguration, List String )
migrate_v1_to_v2 { strategyConfig, removedBuyFilterCount, removedSellFilterCount } =
    let
        v2Strategy =
            removeLegacyConfirmation strategyConfig

        shouldWarnAboutRemovedConfirmation =
            strategyConfig.generalSettings.confirmationSettings /= Rating.defaultCondition

        removedConfirmationWarning =
            if shouldWarnAboutRemovedConfirmation then
                [ "Vaše strategie měla nastaveno Potvrzení investic mobilem, které muselo být odstraněno." ]

            else
                []

        removedBuyFiltersWarning =
            if removedBuyFilterCount > 0 then
                [ pluralizeRules removedBuyFilterCount ++ " nákupu odstraněno, protože obsahovaly zpětně nekompatibilní podmínky" ]

            else
                []

        removedSellFiltersWarning =
            if removedSellFilterCount > 0 then
                [ pluralizeRules removedSellFilterCount ++ " prodeje odstraněno, protože obsahovaly zpětně nekompatibilní podmínky" ]

            else
                []
    in
    ( v2Strategy
    , removedConfirmationWarning ++ removedBuyFiltersWarning ++ removedSellFiltersWarning
    )


pluralizeRules : Int -> String
pluralizeRules x =
    String.fromInt x
        ++ (if x == 1 then
                " pravidlo"

            else if 2 <= x && x <= 4 then
                " pravidla"

            else
                " pravidel"
           )


removeLegacyConfirmation : V1.StrategyConfiguration -> V2.StrategyConfiguration
removeLegacyConfirmation old =
    let
        removeLegacyConfirmation_ : V1.GeneralSettings -> V2.GeneralSettings
        removeLegacyConfirmation_ gs =
            { portfolio = gs.portfolio
            , exitConfig = gs.exitConfig
            , targetPortfolioSize = gs.targetPortfolioSize
            , defaultInvestmentSize = gs.defaultInvestmentSize
            , defaultInvestmentShare = gs.defaultInvestmentShare
            , defaultTargetBalance = gs.defaultTargetBalance
            , confirmationSettings = Confirmation.NoConfirmation
            , reservationSetting = ReservationSetting.Ignore
            }
    in
    { generalSettings = removeLegacyConfirmation_ old.generalSettings
    , portfolioShares = old.portfolioShares
    , investmentSizeOverrides = old.investmentSizeOverrides
    , buyingConfig = old.buyingConfig
    , sellingConfig = old.sellingConfig
    }


versionedStrategyFromUrlHash : UrlHash -> Result String VersionedStrategy
versionedStrategyFromUrlHash hash =
    Base64.decode hash
        |> Result.andThen
            (\versionSemicolonStrategyJson ->
                versionSemicolonStrategyJson
                    |> String.split ";"
                    |> (\pieces ->
                            case pieces of
                                [ versionStr, strategyJson ] ->
                                    case String.toInt versionStr of
                                        Just version ->
                                            decodeStrategy version strategyJson

                                        Nothing ->
                                            Err <| "Failed to read strategy version from " ++ versionStr

                                _ ->
                                    Err <| "Unexpected number of semicolon separated things in " ++ versionSemicolonStrategyJson
                       )
            )


decodeStrategy : Int -> String -> Result String VersionedStrategy
decodeStrategy version strategyJson =
    case version of
        1 ->
            Decode.decodeString V1.strategyDecoder strategyJson
                |> Result.mapError Decode.errorToString
                |> Result.map V1

        2 ->
            Decode.decodeString V2.strategyDecoder strategyJson
                |> Result.mapError Decode.errorToString
                |> Result.map V2

        _ ->
            Err <| "Unsupported strategy version " ++ String.fromInt version
