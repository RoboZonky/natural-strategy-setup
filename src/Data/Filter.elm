module Data.Filter
    exposing
        ( BuyConf(..)
        , BuyingConfiguration(..)
        , FilteredItem(..)
        , MarketplaceEnablement
        , MarketplaceFilter
        , SellConf(..)
        , SellingConfiguration(..)
        , addNegativeCondition
        , addPositiveCondition
        , buyConfRadioLabel
        , decodeBuyingConfiguration
        , decodeSellingConfiguration
        , emptyFilter
        , encodeBuyingConfiguration
        , encodeSellingConfiguration
        , fromBuyConfEnum
        , fromSellConfEnum
        , getFilteredItem
        , isValid
        , itemToPluralString
        , marketplaceFilterValidationErrors
        , renderBuyFilter
        , renderBuyingConfiguration
        , renderFilteredItem
        , renderSellFilter
        , renderSellFilters
        , renderSellingConfiguration
        , sellConfRadioLabel
        , setFilteredItem
        , toBuyConfEnum
        , toSellConfEnum
        , togglePrimaryEnablement
        , toggleSecondaryEnablement
        , updateBuyFilters
        , updateNegativeConditions
        , updatePositiveConditions
        , updateSellFilters
        , validateSellingConfiguration
        )

import Data.Filter.Conditions exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type BuyingConfiguration
    = InvestEverything
    | InvestSomething MarketplaceEnablement (List MarketplaceFilter)
    | InvestNothing



-- TODO This is enum for comparison in radios - any idea how to make it without it?
-- I've got BuyingConfiguration and a radio button representing InvestSomething and I'd like to determine
-- if that radio should be enabled / disabled, but I can't compare wihout pattern matching on constructor..


type BuyConf
    = InvEverything
    | InvSomething
    | InvNothing


toBuyConfEnum : BuyingConfiguration -> BuyConf
toBuyConfEnum buyingConfiguration =
    case buyingConfiguration of
        InvestEverything ->
            InvEverything

        InvestSomething _ _ ->
            InvSomething

        InvestNothing ->
            InvNothing


fromBuyConfEnum : BuyConf -> BuyingConfiguration
fromBuyConfEnum buyConf =
    case buyConf of
        InvEverything ->
            InvestEverything

        InvSomething ->
            InvestSomething (MarketplaceEnablement True True) []

        InvNothing ->
            InvestNothing


buyConfRadioLabel : BuyConf -> String
buyConfRadioLabel bs =
    case bs of
        InvEverything ->
            "Investovat do všech půjček a participací."

        InvSomething ->
            "Investovat do vybraných"

        InvNothing ->
            "Ignorovat všechny půjčky i participace."


updateBuyFilters : (List MarketplaceFilter -> List MarketplaceFilter) -> BuyingConfiguration -> BuyingConfiguration
updateBuyFilters updater buyingConfiguration =
    case buyingConfiguration of
        InvestSomething enablement filters ->
            InvestSomething enablement <| updater filters

        other ->
            other


updateSellFilters : (List MarketplaceFilter -> List MarketplaceFilter) -> SellingConfiguration -> SellingConfiguration
updateSellFilters updater sellingConfiguration =
    case sellingConfiguration of
        SellNothing ->
            SellNothing

        SellSomething filters ->
            SellSomething <| updater filters


togglePrimaryEnablement : Bool -> BuyingConfiguration -> BuyingConfiguration
togglePrimaryEnablement enable buyingConfiguration =
    case buyingConfiguration of
        InvestSomething enablement filters ->
            let
                newEnablement =
                    { enablement | primaryEnabled = enable }

                newFilters =
                    removeDisabledFilters newEnablement filters
            in
            InvestSomething newEnablement newFilters

        other ->
            other


toggleSecondaryEnablement : Bool -> BuyingConfiguration -> BuyingConfiguration
toggleSecondaryEnablement enable buyingConfiguration =
    case buyingConfiguration of
        InvestSomething enablement filters ->
            let
                newEnablement =
                    { enablement | secondaryEnabled = enable }

                newFilters =
                    removeDisabledFilters newEnablement filters
            in
            InvestSomething newEnablement newFilters

        other ->
            other


removeDisabledFilters : MarketplaceEnablement -> List MarketplaceFilter -> List MarketplaceFilter
removeDisabledFilters enablement =
    List.filter (enablementAllowsFilter enablement)


enablementAllowsFilter : MarketplaceEnablement -> MarketplaceFilter -> Bool
enablementAllowsFilter { primaryEnabled, secondaryEnabled } f =
    case ( primaryEnabled, secondaryEnabled ) of
        ( True, True ) ->
            True

        ( True, False ) ->
            f.whatToFilter == Loan

        ( False, True ) ->
            f.whatToFilter == Participation

        ( False, False ) ->
            False


type alias MarketplaceEnablement =
    { primaryEnabled : Bool
    , secondaryEnabled : Bool
    }


type SellingConfiguration
    = SellNothing
    | SellSomething (List MarketplaceFilter)


validateSellingConfiguration : SellingConfiguration -> List String
validateSellingConfiguration sellingConfiguration =
    case sellingConfiguration of
        SellSomething filterList ->
            Util.validate (List.isEmpty filterList) "Musíte přidat alespoň jedno pravidlo pro prodej, nebo zakázat prodej participací"

        SellNothing ->
            []



-- TODO This is enum for comparison in radios - any idea how to make it without it?


type SellConf
    = SNothing
    | SSomething


toSellConfEnum : SellingConfiguration -> SellConf
toSellConfEnum sellingConfiguration =
    case sellingConfiguration of
        SellNothing ->
            SNothing

        SellSomething _ ->
            SSomething


fromSellConfEnum : SellConf -> SellingConfiguration
fromSellConfEnum sellConf =
    case sellConf of
        SNothing ->
            SellNothing

        SSomething ->
            SellSomething []


sellConfRadioLabel : SellConf -> String
sellConfRadioLabel sellConf =
    case sellConf of
        SSomething ->
            "Prodávat vybrané."

        SNothing ->
            "Neprodávat žádné participace."


renderSellingConfiguration : SellingConfiguration -> String
renderSellingConfiguration sellingConfiguration =
    case sellingConfiguration of
        SellNothing ->
            "Prodej participací zakázán."

        SellSomething filters ->
            renderSellFilters filters


type alias MarketplaceFilter =
    { whatToFilter : FilteredItem
    , ignoreWhen : Conditions
    , butNotWhen : Conditions
    }


emptyFilter : MarketplaceFilter
emptyFilter =
    { whatToFilter = Loan
    , ignoreWhen = emptyConditions
    , butNotWhen = emptyConditions
    }


renderBuyingConfiguration : BuyingConfiguration -> String
renderBuyingConfiguration buyingConfiguration =
    case buyingConfiguration of
        InvestEverything ->
            "Investovat do všech půjček a participací."

        InvestSomething enablement filters ->
            renderFilters "\n- Filtrování tržiště" enablement renderBuyFilter filters

        InvestNothing ->
            "Ignorovat všechny půjčky i participace."


renderSellFilters : List MarketplaceFilter -> String
renderSellFilters filters =
    if List.isEmpty filters then
        ""
    else
        Util.joinNonemptyLines <| "\n- Prodej participací" :: List.map renderSellFilter filters


renderFilters : String -> MarketplaceEnablement -> (MarketplaceFilter -> String) -> List MarketplaceFilter -> String
renderFilters heading { primaryEnabled, secondaryEnabled } filterRenderer filters =
    let
        primaryFilters =
            List.filter (\f -> .whatToFilter f == Loan) filters

        secondaryFilters =
            List.filter (\f -> .whatToFilter f == Participation) filters

        bothFilters =
            List.filter (\f -> .whatToFilter f == Loan_And_Participation) filters

        primaryEmpty =
            List.isEmpty <| primaryFilters ++ bothFilters

        secondaryEmpty =
            List.isEmpty <| secondaryFilters ++ bothFilters

        primaryEnablement =
            if primaryEmpty then
                renderPrimaryEnablement primaryEnabled
            else
                ""

        secondaryEnablement =
            if secondaryEmpty then
                renderSecondaryEnablement secondaryEnabled
            else
                ""
    in
    Util.joinNonemptyLines <| heading :: primaryEnablement :: List.map filterRenderer filters ++ [ secondaryEnablement ]


renderPrimaryEnablement : Bool -> String
renderPrimaryEnablement isEnabled =
    case isEnabled of
        True ->
            "Investovat do všech půjček."

        False ->
            "Ignorovat všechny půjčky."


renderSecondaryEnablement : Bool -> String
renderSecondaryEnablement isEnabled =
    case isEnabled of
        True ->
            "Investovat do všech participací."

        False ->
            "Ignorovat všechny participace."


isValid : MarketplaceFilter -> Bool
isValid =
    List.isEmpty << marketplaceFilterValidationErrors


marketplaceFilterValidationErrors : MarketplaceFilter -> List String
marketplaceFilterValidationErrors mf =
    let
        atLeastOnePositiveCondition =
            Util.validate (List.isEmpty <| conditionsToList mf.ignoreWhen) "Pravidlo musí obsahovat aspoň jednu podmínku"
    in
    atLeastOnePositiveCondition
        ++ conditionsValidationErrors "" mf.ignoreWhen
        ++ conditionsValidationErrors "Výjimka - " mf.butNotWhen


setFilteredItem : FilteredItem -> MarketplaceFilter -> MarketplaceFilter
setFilteredItem newItem mf =
    { mf | whatToFilter = newItem }


getFilteredItem : MarketplaceFilter -> FilteredItem
getFilteredItem { whatToFilter } =
    whatToFilter


addPositiveCondition : Condition -> MarketplaceFilter -> MarketplaceFilter
addPositiveCondition c =
    updatePositiveConditions (addCondition c)


addNegativeCondition : Condition -> MarketplaceFilter -> MarketplaceFilter
addNegativeCondition c =
    updateNegativeConditions (addCondition c)


updatePositiveConditions : (Conditions -> Conditions) -> MarketplaceFilter -> MarketplaceFilter
updatePositiveConditions conditionsUpdater mf =
    { mf | ignoreWhen = conditionsUpdater mf.ignoreWhen }


updateNegativeConditions : (Conditions -> Conditions) -> MarketplaceFilter -> MarketplaceFilter
updateNegativeConditions conditionsUpdater mf =
    { mf | butNotWhen = conditionsUpdater mf.butNotWhen }


renderBuyFilter : MarketplaceFilter -> String
renderBuyFilter mf =
    "Ignorovat " ++ renderFilteredItem (getFilteredItem mf) ++ renderCommonPartOfBuyAndSellFilters mf


renderSellFilter : MarketplaceFilter -> String
renderSellFilter mf =
    "Prodat participaci" ++ renderCommonPartOfBuyAndSellFilters mf


renderCommonPartOfBuyAndSellFilters : MarketplaceFilter -> String
renderCommonPartOfBuyAndSellFilters { ignoreWhen, butNotWhen } =
    let
        negativePart =
            if List.isEmpty (conditionsToList butNotWhen) then
                ""
            else
                "\n(Ale ne když: " ++ renderConditionList (conditionsToList butNotWhen) ++ ")"

        positivePart =
            renderConditionList <| conditionsToList ignoreWhen
    in
    ", kde: " ++ positivePart ++ negativePart


type FilteredItem
    = Loan
    | Participation
    | Loan_And_Participation
    | Participation_To_Sell


allFilteredItems : List FilteredItem
allFilteredItems =
    [ Loan, Participation, Loan_And_Participation, Participation_To_Sell ]


renderFilteredItem : FilteredItem -> String
renderFilteredItem item =
    case item of
        Loan_And_Participation ->
            "vše"

        Participation ->
            "participaci"

        Loan ->
            "úvěr"

        Participation_To_Sell ->
            "participace na prodej"


itemToPluralString : FilteredItem -> String
itemToPluralString item =
    case item of
        Loan ->
            "Půjčky"

        Participation ->
            "Participace"

        Loan_And_Participation ->
            "Půjčky i participace"

        Participation_To_Sell ->
            "Participace na prodej"



-- JSON


encodeSellingConfiguration : SellingConfiguration -> Value
encodeSellingConfiguration sellingConfiguratin =
    case sellingConfiguratin of
        SellNothing ->
            Encode.object
                [ ( "strat", Encode.int 0 )
                , ( "filters", Encode.null )
                ]

        SellSomething filters ->
            Encode.object
                [ ( "strat", Encode.int 1 )
                , ( "filters", Encode.list <| List.map encodeMarketplaceFilter filters )
                ]


decodeSellingConfiguration : Decoder SellingConfiguration
decodeSellingConfiguration =
    Decode.field "strat" Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    0 ->
                        Decode.succeed SellNothing

                    1 ->
                        Decode.map SellSomething
                            (Decode.field "filters" (Decode.list marketplaceFilterDecoder))

                    _ ->
                        Decode.fail <| "Unable to SellingConfiguration from " ++ toString x
            )


encodeBuyingConfiguration : BuyingConfiguration -> Value
encodeBuyingConfiguration buyingConfiguration =
    case buyingConfiguration of
        InvestEverything ->
            Encode.object
                [ ( "strat", Encode.int 0 )
                , ( "filters", Encode.null )
                ]

        InvestSomething enablement filters ->
            Encode.object
                [ ( "strat", Encode.int 1 )
                , ( "primEnabled", Encode.bool enablement.primaryEnabled )
                , ( "secEnabled", Encode.bool enablement.secondaryEnabled )
                , ( "filters", Encode.list <| List.map encodeMarketplaceFilter filters )
                ]

        InvestNothing ->
            Encode.object
                [ ( "strat", Encode.int 2 )
                , ( "filters", Encode.null )
                ]


decodeBuyingConfiguration : Decoder BuyingConfiguration
decodeBuyingConfiguration =
    Decode.field "strat" Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    0 ->
                        Decode.succeed InvestEverything

                    1 ->
                        Decode.map2 InvestSomething
                            (Decode.map2 MarketplaceEnablement
                                (Decode.field "primEnabled" Decode.bool)
                                (Decode.field "secEnabled" Decode.bool)
                            )
                            (Decode.field "filters" (Decode.list marketplaceFilterDecoder))

                    2 ->
                        Decode.succeed InvestNothing

                    _ ->
                        Decode.fail <| "Unable to decode BuyingConfiguration from " ++ toString x
            )


encodeFilteredItem : FilteredItem -> Value
encodeFilteredItem =
    Encode.string << toString


encodeMarketplaceFilter : MarketplaceFilter -> Value
encodeMarketplaceFilter { whatToFilter, ignoreWhen, butNotWhen } =
    Encode.object
        [ ( "whatToFilter", encodeFilteredItem whatToFilter )
        , ( "ignoreWhen", encodeConditions ignoreWhen )
        , ( "butNotWhen", encodeConditions butNotWhen )
        ]


filteredItemDecoder : Decoder FilteredItem
filteredItemDecoder =
    Util.enumDecoder allFilteredItems


marketplaceFilterDecoder : Decoder MarketplaceFilter
marketplaceFilterDecoder =
    Decode.map3 MarketplaceFilter
        (Decode.field "whatToFilter" filteredItemDecoder)
        (Decode.field "ignoreWhen" conditionsDecoder)
        (Decode.field "butNotWhen" conditionsDecoder)
