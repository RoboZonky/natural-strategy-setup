module Data.VersionedStrategy exposing (loadStrategy)

import Base64
import Data.Migration.Migration as Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V1 as V1
import Data.Migration.Strategy.V2 as V2
import Data.Migration.Strategy.V3 as V3
import Data.Migration.Strategy.V4 as V4
import Data.Migration.Strategy.V5 as V5
import Data.Migration.Strategy.V6 as V6
import Data.Strategy as V6
import Json.Decode as Decode
import Types exposing (UrlHash)


type VersionedStrategy
    = V1 V1.DecodedStrategy
    | V2 V2.StrategyConfiguration
    | V3 V3.StrategyConfiguration
    | V4 V4.StrategyConfiguration
    | V5 V5.StrategyConfiguration
    | V6 V6.StrategyConfiguration


type alias StrategyJson =
    String


loadStrategy : UrlHash -> Result String ( V6.StrategyConfiguration, List MigrationWarning )
loadStrategy hash =
    case versionedStrategyFromUrlHash hash of
        Ok (V1 v1) ->
            V2.fromV1 v1
                |> Migration.andThen V3.fromV2
                |> Migration.andThen V4.fromV3
                |> Migration.andThen V5.fromV4
                |> Migration.andThen V6.fromV5
                |> Ok

        Ok (V2 v2) ->
            V3.fromV2 v2
                |> Migration.andThen V4.fromV3
                |> Migration.andThen V5.fromV4
                |> Migration.andThen V6.fromV5
                |> Ok

        Ok (V3 v3) ->
            V4.fromV3 v3
                |> Migration.andThen V5.fromV4
                |> Migration.andThen V6.fromV5
                |> Ok

        Ok (V4 v4) ->
            V5.fromV4 v4
                |> Migration.andThen V6.fromV5
                |> Ok

        Ok (V5 v5) ->
            V6.fromV5 v5 |> Ok

        Ok (V6 v6) ->
            Ok ( v6, [] )

        Err e ->
            Err e


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


decodeStrategy : Int -> StrategyJson -> Result String VersionedStrategy
decodeStrategy version strategyJson =
    case version of
        1 ->
            decodeVersionedStrategy V1.strategyDecoder V1 strategyJson

        2 ->
            decodeVersionedStrategy V2.strategyDecoder V2 strategyJson

        3 ->
            decodeVersionedStrategy V3.strategyDecoder V3 strategyJson

        4 ->
            decodeVersionedStrategy V4.strategyDecoder V4 strategyJson

        5 ->
            decodeVersionedStrategy V5.strategyDecoder V5 strategyJson

        6 ->
            decodeVersionedStrategy V6.strategyDecoder V6 strategyJson

        _ ->
            Err <| "Unsupported strategy version " ++ String.fromInt version


decodeVersionedStrategy : Decode.Decoder strat -> (strat -> VersionedStrategy) -> StrategyJson -> Result String VersionedStrategy
decodeVersionedStrategy versionedStrategyDecoder inject strategyJson =
    Decode.decodeString versionedStrategyDecoder strategyJson
        |> Result.mapError Decode.errorToString
        |> Result.map inject
