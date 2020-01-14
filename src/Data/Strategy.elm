module Data.Strategy exposing
    ( GeneralSettings
    , StrategyConfiguration
    , addBuyFilter
    , addSellFilter
    , defaultStrategyConfiguration
    , generalSettingsDecoder
    , removeBuyFilter
    , removeSellFilter
    , renderStrategyConfiguration
    , setBuyingConfiguration
    , setDefaultInvestment
    , setDefaultInvestmentShare
    , setExitConfig
    , setInvestment
    , setPortfolio
    , setPortfolioSharePercentage
    , setReservationSetting
    , setSellingConfiguration
    , setTargetPortfolioSize
    , strategyDecoder
    , strategyEqual
    , strategyToUrlHash
    , togglePrimaryMarket
    , toggleSecondaryMarket
    , validateStrategyConfiguration
    )

import Base64
import Data.ExitConfig as ExitConfig exposing (ExitConfig)
import Data.Filter as Filters exposing (BuyingConfiguration, MarketplaceFilter, SellingConfiguration)
import Data.Filter.Conditions.Rating exposing (Rating(..))
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioStructure)
import Data.ReservationSetting as ReservationSetting exposing (ReservationSetting)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Dict.Any
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Percentage
import RangeSlider
import Time exposing (Posix)
import Types exposing (BaseUrl, UrlHash)
import Util
import Version


type alias StrategyConfiguration =
    { generalSettings : GeneralSettings
    , portfolioStructure : PortfolioStructure
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
    , reservationSetting : ReservationSetting
    }


defaultStrategyConfiguration : StrategyConfiguration
defaultStrategyConfiguration =
    { generalSettings =
        { portfolio = Portfolio.Conservative
        , exitConfig = ExitConfig.DontExit
        , targetPortfolioSize = TargetPortfolioSize.NotSpecified
        , defaultInvestmentSize = Investment.defaultSize
        , defaultInvestmentShare = InvestmentShare.NotSpecified
        , reservationSetting = ReservationSetting.defaultSetting
        }
    , portfolioStructure = PortfolioStructure.conservative
    , investmentSizeOverrides = Investment.defaultInvestmentsPerRating Investment.defaultSize
    , buyingConfig = Filters.InvestEverything
    , sellingConfig = Filters.SellNothing
    }


setPortfolio : Portfolio -> StrategyConfiguration -> StrategyConfiguration
setPortfolio portfolio strategy =
    let
        portfolioShares =
            case portfolio of
                Conservative ->
                    PortfolioStructure.conservative

                Balanced ->
                    PortfolioStructure.balanced

                Progressive ->
                    PortfolioStructure.progressive

                UserDefined ->
                    {- switch to UserDefined leaves the current slider configuration untouched -}
                    strategy.portfolioStructure
    in
    case strategy of
        { generalSettings } as settings ->
            { settings
                | generalSettings = { generalSettings | portfolio = portfolio }
                , portfolioStructure = portfolioShares
            }


setExitConfig : ExitConfig -> StrategyConfiguration -> StrategyConfiguration
setExitConfig exitConfig ({ generalSettings } as config) =
    { config | generalSettings = { generalSettings | exitConfig = exitConfig } }


setTargetPortfolioSize : TargetPortfolioSize -> StrategyConfiguration -> StrategyConfiguration
setTargetPortfolioSize targetPortfolioSize ({ generalSettings } as config) =
    { config | generalSettings = { generalSettings | targetPortfolioSize = targetPortfolioSize } }


setDefaultInvestmentShare : InvestmentShare -> StrategyConfiguration -> StrategyConfiguration
setDefaultInvestmentShare share ({ generalSettings } as config) =
    { config | generalSettings = { generalSettings | defaultInvestmentShare = share } }


setPortfolioSharePercentage : Rating -> Percentage.Msg -> StrategyConfiguration -> StrategyConfiguration
setPortfolioSharePercentage rtg msg config =
    let
        updatePortfolioStructure : PortfolioStructure -> PortfolioStructure
        updatePortfolioStructure =
            Dict.Any.update rtg (Maybe.map (Percentage.update msg))
    in
    { config | portfolioStructure = updatePortfolioStructure config.portfolioStructure }


setInvestment : Rating -> RangeSlider.Msg -> StrategyConfiguration -> StrategyConfiguration
setInvestment rtg msg config =
    let
        investmentUpdater : InvestmentsPerRating -> InvestmentsPerRating
        investmentUpdater =
            Dict.Any.update rtg (Maybe.map (RangeSlider.update msg))
    in
    { config | investmentSizeOverrides = investmentUpdater config.investmentSizeOverrides }


setDefaultInvestment : RangeSlider.Msg -> StrategyConfiguration -> StrategyConfiguration
setDefaultInvestment msg config =
    let
        setDefaultInvestmentHelper : GeneralSettings -> GeneralSettings
        setDefaultInvestmentHelper generalSettings =
            { generalSettings | defaultInvestmentSize = RangeSlider.update msg generalSettings.defaultInvestmentSize }

        newGeneralSettings =
            setDefaultInvestmentHelper config.generalSettings
    in
    { config
        | generalSettings = newGeneralSettings
        , investmentSizeOverrides = Investment.defaultInvestmentsPerRating newGeneralSettings.defaultInvestmentSize
    }


setReservationSetting : ReservationSetting -> StrategyConfiguration -> StrategyConfiguration
setReservationSetting reservationSetting ({ generalSettings } as config) =
    { config
        | generalSettings = { generalSettings | reservationSetting = reservationSetting }
    }


removeBuyFilter : Int -> StrategyConfiguration -> StrategyConfiguration
removeBuyFilter index config =
    { config | buyingConfig = Filters.updateBuyFilters (List.Extra.removeAt index) config.buyingConfig }


removeSellFilter : Int -> StrategyConfiguration -> StrategyConfiguration
removeSellFilter index config =
    { config | sellingConfig = Filters.removeSellFilterAt index config.sellingConfig }


setBuyingConfiguration : Filters.BuyingConfiguration -> StrategyConfiguration -> StrategyConfiguration
setBuyingConfiguration buyingConfiguration strategy =
    { strategy | buyingConfig = buyingConfiguration }


setSellingConfiguration : Filters.SellingConfiguration -> StrategyConfiguration -> StrategyConfiguration
setSellingConfiguration sellingConfiguration strategy =
    { strategy | sellingConfig = sellingConfiguration }


togglePrimaryMarket : Bool -> StrategyConfiguration -> StrategyConfiguration
togglePrimaryMarket enable strategy =
    { strategy | buyingConfig = Filters.togglePrimaryEnablement enable strategy.buyingConfig }


toggleSecondaryMarket : Bool -> StrategyConfiguration -> StrategyConfiguration
toggleSecondaryMarket enable strategy =
    { strategy | buyingConfig = Filters.toggleSecondaryEnablement enable strategy.buyingConfig }


addBuyFilter : MarketplaceFilter -> StrategyConfiguration -> StrategyConfiguration
addBuyFilter newFilter config =
    { config | buyingConfig = Filters.updateBuyFilters (\fs -> fs ++ [ newFilter ]) config.buyingConfig }


addSellFilter : MarketplaceFilter -> StrategyConfiguration -> StrategyConfiguration
addSellFilter newFilter config =
    { config | sellingConfig = Filters.addSellFilter newFilter config.sellingConfig }


renderStrategyConfiguration : BaseUrl -> Posix -> StrategyConfiguration -> String
renderStrategyConfiguration baseUrl generatedOn ({ generalSettings, portfolioStructure, investmentSizeOverrides, buyingConfig, sellingConfig } as strategyConfig) =
    Util.joinNonemptyLines
        [ Version.strategyComment generatedOn
        , Version.robozonkyVersionStatement
        , renderGeneralSettings generalSettings
        , PortfolioStructure.renderPortfolioStructure generalSettings.portfolio portfolioStructure
        , Investment.renderInvestments generalSettings.defaultInvestmentSize investmentSizeOverrides
        , Filters.renderBuyingConfiguration buyingConfig
        , Filters.renderSellingConfiguration sellingConfig
        , shareableUrlComment baseUrl strategyConfig
        ]


renderGeneralSettings : GeneralSettings -> String
renderGeneralSettings generalSettings =
    Util.joinNonemptyLines
        [ "- Obecná nastavení"
        , Portfolio.render generalSettings.portfolio
        , ReservationSetting.render generalSettings.reservationSetting
        , ExitConfig.render generalSettings.exitConfig
        , TargetPortfolioSize.render generalSettings.targetPortfolioSize
        , Investment.renderSize generalSettings.defaultInvestmentSize
        , InvestmentShare.render generalSettings.defaultInvestmentShare
        ]


validateStrategyConfiguration : StrategyConfiguration -> List String
validateStrategyConfiguration strategyConfig =
    List.concat
        [ validateGeneralSettings strategyConfig.generalSettings
        , PortfolioStructure.validate strategyConfig.portfolioStructure
        , Filters.validateSellingConfiguration strategyConfig.sellingConfig
        ]


validateGeneralSettings : GeneralSettings -> List String
validateGeneralSettings generalSettings =
    List.concat
        [ ExitConfig.validate generalSettings.exitConfig
        , TargetPortfolioSize.validate generalSettings.targetPortfolioSize
        , InvestmentShare.validate generalSettings.defaultInvestmentShare
        ]


{-| Strategy equality
-}
strategyEqual : StrategyConfiguration -> StrategyConfiguration -> Bool
strategyEqual s1 s2 =
    Util.and
        [ generalSettingsEqual s1.generalSettings s2.generalSettings
        , PortfolioStructure.portfolioStructureEqual s1.portfolioStructure s2.portfolioStructure
        , Investment.investmentsPerRatingEqual s1.investmentSizeOverrides s2.investmentSizeOverrides
        , s1.buyingConfig == s2.buyingConfig
        , s1.sellingConfig == s2.sellingConfig
        ]


generalSettingsEqual : GeneralSettings -> GeneralSettings -> Bool
generalSettingsEqual gs1 gs2 =
    Util.and
        [ gs1.portfolio == gs2.portfolio
        , gs1.exitConfig == gs2.exitConfig
        , gs1.targetPortfolioSize == gs2.targetPortfolioSize
        , Investment.investmentSizeEqual gs1.defaultInvestmentSize gs2.defaultInvestmentSize
        , gs1.defaultInvestmentShare == gs2.defaultInvestmentShare
        ]



-- JSON


encodeGeneralSettings : GeneralSettings -> Value
encodeGeneralSettings { portfolio, exitConfig, targetPortfolioSize, defaultInvestmentSize, defaultInvestmentShare, reservationSetting } =
    Encode.object
        [ ( "a", Portfolio.encode portfolio )
        , ( "b", ExitConfig.encode exitConfig )
        , ( "c", TargetPortfolioSize.encode targetPortfolioSize )
        , ( "d", Investment.encodeSize defaultInvestmentSize )
        , ( "e", InvestmentShare.encode defaultInvestmentShare )
        , ( "g1", ReservationSetting.encode reservationSetting )
        ]


generalSettingsDecoder : Decoder GeneralSettings
generalSettingsDecoder =
    Decode.map6 GeneralSettings
        (Decode.field "a" Portfolio.decoder)
        (Decode.field "b" ExitConfig.decoder)
        (Decode.field "c" TargetPortfolioSize.decoder)
        (Decode.field "d" Investment.sizeDecoder)
        (Decode.field "e" InvestmentShare.decoder)
        (Decode.field "g1" ReservationSetting.decoder)


encodeStrategy : StrategyConfiguration -> Value
encodeStrategy { generalSettings, portfolioStructure, investmentSizeOverrides, buyingConfig, sellingConfig } =
    let
        maybePortfolioStructure =
            {- Only encode portfolio structure for user defined portfolios -}
            case generalSettings.portfolio of
                UserDefined ->
                    [ ( "i", PortfolioStructure.encode portfolioStructure ) ]

                _ ->
                    []
    in
    Encode.object <|
        maybePortfolioStructure
            ++ [ ( "h", encodeGeneralSettings generalSettings )
               , ( "j", Investment.encode investmentSizeOverrides )
               , ( "k", Filters.encodeBuyingConfiguration buyingConfig )
               , ( "l", Filters.encodeSellingConfiguration sellingConfig )
               ]


strategyDecoder : Decoder StrategyConfiguration
strategyDecoder =
    {- Need the portfolio ahead of time because it determines if we
       should decode portfolio structure (for UserDefined) or use one of the predefined ones
    -}
    Decode.field "h" generalSettingsDecoder
        |> Decode.andThen
            (\generalSettings ->
                Decode.map4 (StrategyConfiguration generalSettings)
                    (PortfolioStructure.decoderFromPortfolio generalSettings.portfolio)
                    (Decode.field "j" Investment.decoder)
                    (Decode.field "k" Filters.decodeBuyingConfiguration)
                    (Decode.field "l" Filters.decodeSellingConfiguration)
            )


strategyToUrlHash : StrategyConfiguration -> UrlHash
strategyToUrlHash strategyConfiguration =
    encodeStrategy strategyConfiguration
        |> Encode.encode 0
        |> (\strategyJson -> String.fromInt strategyVersion ++ ";" ++ strategyJson)
        |> Base64.encode


shareableUrlComment : BaseUrl -> StrategyConfiguration -> String
shareableUrlComment baseUrl strategyConfig =
    let
        urlHash =
            strategyToUrlHash strategyConfig
    in
    String.join "\n"
        [ "# ----------------------------------------------------------------------"
        , "# Pro budoucí úpravy této strategie vložte následující URL do prohlížeče"

        {- This line has to end with a newline, so it's accepted by RoboZonky parser as a comment -}
        , "# " ++ baseUrl ++ "#" ++ urlHash ++ "\n"
        ]


strategyVersion : Int
strategyVersion =
    6
