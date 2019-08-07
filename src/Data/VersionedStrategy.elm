module Data.VersionedStrategy exposing (loadStrategy)

import Base64
import Data.Confirmation as Confirmation
import Data.Migration.StrategyV1 as V1
import Data.Migration.StrategyV2 as V2
import Data.Strategy as V3
import Json.Decode as Decode
import Types exposing (UrlHash)


type VersionedStrategy
    = V1 V1.DecodedStrategy
    | V2 V2.StrategyConfiguration
    | V3 V3.StrategyConfiguration


loadStrategy : UrlHash -> Result String ( V3.StrategyConfiguration, List String )
loadStrategy hash =
    case versionedStrategyFromUrlHash hash of
        Ok (V1 v1) ->
            let
                ( v2, warnings2 ) =
                    V2.fromV1 v1

                ( v3, warnings3 ) =
                    fromV2 v2
            in
            Ok ( v3, warnings2 ++ warnings3 )

        Ok (V2 v2) ->
            Ok (fromV2 v2)

        Ok (V3 v3) ->
            Ok ( v3, [] )

        Err e ->
            Err e


fromV2 : V2.StrategyConfiguration -> ( V3.StrategyConfiguration, List String )
fromV2 old =
    let
        shouldWarnAboutRemovedConfirmation =
            old.generalSettings.confirmationSettings /= Confirmation.defaultSettings

        perhapsWarning =
            if shouldWarnAboutRemovedConfirmation then
                [ "Vaše strategie měla nastaveno Potvrzení investic mobilem, které muselo být odstraněno." ]

            else
                []
    in
    ( removeConfirmationSettings old, perhapsWarning )


removeConfirmationSettings : V2.StrategyConfiguration -> V3.StrategyConfiguration
removeConfirmationSettings old =
    let
        removeConfirmationSettings_ : V2.GeneralSettings -> V3.GeneralSettings
        removeConfirmationSettings_ gs =
            { portfolio = gs.portfolio
            , exitConfig = gs.exitConfig
            , targetPortfolioSize = gs.targetPortfolioSize
            , defaultInvestmentSize = gs.defaultInvestmentSize
            , defaultInvestmentShare = gs.defaultInvestmentShare
            , defaultTargetBalance = gs.defaultTargetBalance
            , reservationSetting = gs.reservationSetting
            }
    in
    { generalSettings = removeConfirmationSettings_ old.generalSettings
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

        3 ->
            Decode.decodeString V3.strategyDecoder strategyJson
                |> Result.mapError Decode.errorToString
                |> Result.map V3

        _ ->
            Err <| "Unsupported strategy version " ++ String.fromInt version
