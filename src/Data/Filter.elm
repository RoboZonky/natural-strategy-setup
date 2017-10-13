module Data.Filter
    exposing
        ( FilteredItem(..)
        , MarketplaceFilter
        , addNegativeCondition
        , addPositiveCondition
        , emptyFilter
        , encodeMarketplaceFilter
        , getFilteredItem
        , isValid
        , itemToPluralString
        , marketplaceFilterDecoder
        , marketplaceFilterValidationErrors
        , renderBuyFilter
        , renderBuyFilters
        , renderFilteredItem
        , renderSellFilter
        , renderSellFilters
        , setFilteredItem
        , updateNegativeConditions
        , updatePositiveConditions
        )

import Data.Filter.Conditions exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


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


renderBuyFilters : List MarketplaceFilter -> String
renderBuyFilters =
    renderFilters "\n- Filtrování tržiště" renderBuyFilter


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
