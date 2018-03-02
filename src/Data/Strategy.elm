module Data.Strategy
    exposing
        ( GeneralSettings
        , StrategyConfiguration
        , addBuyFilter
        , addSellFilter
        , defaultStrategyConfiguration
        , removeBuyFilter
        , removeSellFilter
        , renderStrategyConfiguration
        , setBuyConf
        , setBuyingConfiguration
        , setDefaultInvestment
        , setDefaultInvestmentShare
        , setExitConfig
        , setInvestment
        , setPortfolio
        , setPortfolioShareRange
        , setSellConf
        , setSellingConfiguration
        , setTargetBalance
        , setTargetPortfolioSize
        , strategyEqual
        , strategyFromUrlHash
        , strategyToUrlHash
        , togglePrimaryMarket
        , toggleSecondaryMarket
        , updateNotificationSettings
        , validateStrategyConfiguration
        )

import AllDict
import Base64
import Data.Confirmation as Confirmation exposing (ConfirmationSettings)
import Data.ExitConfig as ExitConfig exposing (ExitConfig)
import Data.Filter as Filters exposing (BuyingConfiguration, MarketplaceFilter, SellingConfiguration)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..), RatingMsg)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.PortfolioStructure.PredefinedShares as PredefinedShares
import Data.TargetBalance as TargetBalance exposing (TargetBalance)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import RangeSlider
import Time.Date exposing (Date)
import Types exposing (BaseUrl)
import Util
import Version


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
    , confirmationSettings : ConfirmationSettings
    }


defaultStrategyConfiguration : StrategyConfiguration
defaultStrategyConfiguration =
    { generalSettings =
        { portfolio = Portfolio.Conservative
        , exitConfig = ExitConfig.DontExit
        , targetPortfolioSize = TargetPortfolioSize.NotSpecified
        , defaultInvestmentSize = Investment.defaultSize
        , defaultInvestmentShare = InvestmentShare.NotSpecified
        , defaultTargetBalance = TargetBalance.NotSpecified
        , confirmationSettings = Confirmation.confirmationsDisabled
        }
    , portfolioShares = PredefinedShares.conservative
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
                    PredefinedShares.conservative

                Balanced ->
                    PredefinedShares.balanced

                Progressive ->
                    PredefinedShares.progressive

                Empty ->
                    PredefinedShares.empty
    in
    case strategy of
        { generalSettings } as settings ->
            { settings
                | generalSettings = { generalSettings | portfolio = portfolio }
                , portfolioShares = portfolioShares
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


updateNotificationSettings : RatingMsg -> StrategyConfiguration -> StrategyConfiguration
updateNotificationSettings msg ({ generalSettings } as config) =
    { config
        | generalSettings = { generalSettings | confirmationSettings = Rating.update msg generalSettings.confirmationSettings }
    }


setPortfolioShareRange : Rating -> RangeSlider.Msg -> StrategyConfiguration -> StrategyConfiguration
setPortfolioShareRange rtg msg config =
    let
        sharesUpdater : PortfolioShares -> PortfolioShares
        sharesUpdater =
            AllDict.update rtg (Maybe.map (RangeSlider.update msg))
    in
    { config | portfolioShares = sharesUpdater config.portfolioShares }


setInvestment : Rating -> RangeSlider.Msg -> StrategyConfiguration -> StrategyConfiguration
setInvestment rtg msg config =
    let
        investmentUpdater : InvestmentsPerRating -> InvestmentsPerRating
        investmentUpdater =
            AllDict.update rtg (Maybe.map (RangeSlider.update msg))
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


setTargetBalance : TargetBalance -> StrategyConfiguration -> StrategyConfiguration
setTargetBalance newBalance ({ generalSettings } as config) =
    { config
        | generalSettings = { generalSettings | defaultTargetBalance = newBalance }
    }


removeBuyFilter : Int -> StrategyConfiguration -> StrategyConfiguration
removeBuyFilter index config =
    { config | buyingConfig = Filters.updateBuyFilters (List.Extra.removeAt index) config.buyingConfig }


removeSellFilter : Int -> StrategyConfiguration -> StrategyConfiguration
removeSellFilter index config =
    { config | sellingConfig = Filters.updateSellFilters (List.Extra.removeAt index) config.sellingConfig }


setBuyingConfiguration : Filters.BuyingConfiguration -> StrategyConfiguration -> StrategyConfiguration
setBuyingConfiguration buyingConfiguration strategy =
    { strategy | buyingConfig = buyingConfiguration }


setSellingConfiguration : Filters.SellingConfiguration -> StrategyConfiguration -> StrategyConfiguration
setSellingConfiguration sellingConfiguration strategy =
    { strategy | sellingConfig = sellingConfiguration }


setBuyConf : Filters.BuyConf -> StrategyConfiguration -> StrategyConfiguration
setBuyConf buyConf =
    setBuyingConfiguration (Filters.fromBuyConfEnum buyConf)


setSellConf : Filters.SellConf -> StrategyConfiguration -> StrategyConfiguration
setSellConf sellConf =
    setSellingConfiguration (Filters.fromSellConfEnum sellConf)


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
    { config | sellingConfig = Filters.updateSellFilters (\fs -> fs ++ [ newFilter ]) config.sellingConfig }


renderStrategyConfiguration : BaseUrl -> Date -> StrategyConfiguration -> String
renderStrategyConfiguration baseUrl generatedOn ({ generalSettings, portfolioShares, investmentSizeOverrides, buyingConfig, sellingConfig } as strategyConfig) =
    Util.joinNonemptyLines
        [ Version.strategyComment generatedOn
        , Version.robozonkyVersionStatement
        , renderGeneralSettings generalSettings
        , PortfolioStructure.renderPortfolioShares generalSettings.portfolio portfolioShares
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
        , ExitConfig.render generalSettings.exitConfig
        , TargetPortfolioSize.render generalSettings.targetPortfolioSize
        , Investment.renderSize generalSettings.defaultInvestmentSize
        , InvestmentShare.render generalSettings.defaultInvestmentShare
        , TargetBalance.render generalSettings.defaultTargetBalance
        , Confirmation.render generalSettings.confirmationSettings
        ]


validateStrategyConfiguration : StrategyConfiguration -> List String
validateStrategyConfiguration strategyConfig =
    List.concat
        [ validateGeneralSettings strategyConfig.generalSettings
        , PortfolioStructure.validate strategyConfig.portfolioShares
        , Filters.validateSellingConfiguration strategyConfig.sellingConfig
        ]


validateGeneralSettings : GeneralSettings -> List String
validateGeneralSettings generalSettings =
    List.concat
        [ ExitConfig.validate generalSettings.exitConfig
        , TargetPortfolioSize.validate generalSettings.targetPortfolioSize
        , InvestmentShare.validate generalSettings.defaultInvestmentShare
        , TargetBalance.validate generalSettings.defaultTargetBalance
        ]


{-| Strategy equality
-}
strategyEqual : StrategyConfiguration -> StrategyConfiguration -> Bool
strategyEqual s1 s2 =
    Util.and
        [ generalSettingsEqual s1.generalSettings s2.generalSettings
        , PortfolioStructure.portfolioSharesEqual s1.portfolioShares s2.portfolioShares
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
        , gs1.defaultTargetBalance == gs2.defaultTargetBalance
        , Confirmation.equal gs1.confirmationSettings gs2.confirmationSettings
        ]



-- JSON


encodeGeneralSettings : GeneralSettings -> Value
encodeGeneralSettings { portfolio, exitConfig, targetPortfolioSize, defaultInvestmentSize, defaultInvestmentShare, defaultTargetBalance, confirmationSettings } =
    Encode.object
        [ ( "a", Portfolio.encode portfolio )
        , ( "b", ExitConfig.encode exitConfig )
        , ( "c", TargetPortfolioSize.encode targetPortfolioSize )
        , ( "d", Investment.encodeSize defaultInvestmentSize )
        , ( "e", InvestmentShare.encode defaultInvestmentShare )
        , ( "f", TargetBalance.encode defaultTargetBalance )
        , ( "g", Confirmation.encode confirmationSettings )
        ]


generalSettingsDecoder : Decoder GeneralSettings
generalSettingsDecoder =
    Decode.map7 GeneralSettings
        (Decode.field "a" Portfolio.decoder)
        (Decode.field "b" ExitConfig.decoder)
        (Decode.field "c" TargetPortfolioSize.decoder)
        (Decode.field "d" Investment.sizeDecoder)
        (Decode.field "e" InvestmentShare.decoder)
        (Decode.field "f" TargetBalance.decoder)
        (Decode.field "g" Confirmation.decoder)


encodeStrategy : StrategyConfiguration -> Value
encodeStrategy { generalSettings, portfolioShares, investmentSizeOverrides, buyingConfig, sellingConfig } =
    Encode.object
        [ ( "h", encodeGeneralSettings generalSettings )
        , ( "i", PortfolioStructure.encode portfolioShares )
        , ( "j", Investment.encode investmentSizeOverrides )
        , ( "k", Filters.encodeBuyingConfiguration buyingConfig )
        , ( "l", Filters.encodeSellingConfiguration sellingConfig )
        ]


strategyDecoder : Decoder StrategyConfiguration
strategyDecoder =
    Decode.map5 StrategyConfiguration
        (Decode.field "h" generalSettingsDecoder)
        (Decode.field "i" PortfolioStructure.decoder)
        (Decode.field "j" Investment.decoder)
        (Decode.field "k" Filters.decodeBuyingConfiguration)
        (Decode.field "l" Filters.decodeSellingConfiguration)


type alias UrlHash =
    String


strategyToUrlHash : StrategyConfiguration -> UrlHash
strategyToUrlHash strategyConfiguration =
    encodeStrategy strategyConfiguration
        |> Encode.encode 0
        |> (\strategyJson -> "1;" ++ strategyJson)
        |> Base64.encode


strategyFromUrlHash : UrlHash -> Result String StrategyConfiguration
strategyFromUrlHash hash =
    Base64.decode hash
        |> Result.andThen
            (\versionSemicolonStrategyJson ->
                versionSemicolonStrategyJson
                    |> String.split ";"
                    |> (\pieces ->
                            case pieces of
                                [ versionStr, strategyJson ] ->
                                    case String.toInt versionStr of
                                        Ok version ->
                                            pickStrategyDecoder version
                                                |> Result.andThen (\decoder -> Decode.decodeString decoder strategyJson)

                                        Err _ ->
                                            Err ("Failed to read strategy version from " ++ versionStr)

                                _ ->
                                    Err ("Unexpected number of semicolon separated things in " ++ versionSemicolonStrategyJson)
                       )
            )


pickStrategyDecoder : Int -> Result String (Decoder StrategyConfiguration)
pickStrategyDecoder version =
    case version of
        1 ->
            Ok strategyDecoder

        uspupportedVersion ->
            Err ("Unsupported strategy version " ++ toString uspupportedVersion)


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
