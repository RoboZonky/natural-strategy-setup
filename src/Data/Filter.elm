module Data.Filter
    exposing
        ( BuyConf(..)
        , BuyingConfiguration(..)
        , FilteredItem(..)
        , MarketplaceFilter
        , addNegativeCondition
        , addPositiveCondition
        , buyConfRadioLabel
        , decodeBuyingConfiguration
        , emptyFilter
        , encodeBuyingConfiguration
        , encodeMarketplaceFilter
        , fromBuyConfEnum
        , getFilteredItem
        , isValid
        , itemToPluralString
        , marketplaceFilterDecoder
        , marketplaceFilterValidationErrors
        , renderBuyFilter
        , renderBuyingConfiguration
        , renderFilteredItem
        , renderSellFilter
        , renderSellFilters
        , setFilteredItem
        , toBuyConfEnum
        , updateFilters
        , updateNegativeConditions
        , updatePositiveConditions
        )

import Data.Filter.Conditions exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type BuyingConfiguration
    = InvestEverything
    | InvestSomething (List MarketplaceFilter)
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

        InvestSomething _ ->
            InvSomething

        InvestNothing ->
            InvNothing


fromBuyConfEnum : BuyConf -> BuyingConfiguration
fromBuyConfEnum buyConf =
    case buyConf of
        InvEverything ->
            InvestEverything

        InvSomething ->
            InvestSomething []

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
            "Ignorovat primární i sekundární tržiště."


updateFilters : (List MarketplaceFilter -> List MarketplaceFilter) -> BuyingConfiguration -> BuyingConfiguration
updateFilters updater buyingConfiguration =
    case buyingConfiguration of
        InvestSomething filters ->
            InvestSomething <| updater filters

        other ->
            other


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

        InvestSomething filters ->
            renderFilters "\n- Filtrování tržiště" renderBuyFilter filters

        InvestNothing ->
            "Ignorovat primární i sekundární tržiště."


renderSellFilters : List MarketplaceFilter -> String
renderSellFilters =
    renderFilters "\n- Prodej participací" renderSellFilter


renderFilters : String -> (MarketplaceFilter -> String) -> List MarketplaceFilter -> String
renderFilters heading filterRenderer filters =
    if List.isEmpty filters then
        ""
    else
        Util.joinNonemptyLines <| heading :: List.map filterRenderer filters


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


encodeBuyingConfiguration : BuyingConfiguration -> Value
encodeBuyingConfiguration buyingConfiguration =
    case buyingConfiguration of
        InvestEverything ->
            Encode.object
                [ ( "strat", Encode.int 1 )
                , ( "filters", Encode.null )
                ]

        InvestSomething filters ->
            Encode.object
                [ ( "strat", Encode.int 2 )
                , ( "filters", Encode.list <| List.map encodeMarketplaceFilter filters )
                ]

        InvestNothing ->
            Encode.object
                [ ( "strat", Encode.int 3 )
                , ( "filters", Encode.null )
                ]


decodeBuyingConfiguration : Decoder BuyingConfiguration
decodeBuyingConfiguration =
    Decode.field "strat" Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    1 ->
                        Decode.succeed InvestEverything

                    2 ->
                        Decode.map InvestSomething <| Decode.field "filters" (Decode.list marketplaceFilterDecoder)

                    3 ->
                        Decode.succeed InvestNothing

                    _ ->
                        Decode.fail <| "Unable to decode buying strategy from " ++ toString x
            )


encodeFilteredItem : FilteredItem -> Value
encodeFilteredItem =
    Encode.string << toString



-- TODO remove from export list after Buing / selling strategy implemented


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
