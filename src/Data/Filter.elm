module Data.Filter
    exposing
        ( FilteredItem(..)
        , MarketplaceFilter(..)
        , addNegativeCondition
        , addPositiveCondition
        , emptyFilter
        , getFilteredItem
        , isValid
        , itemToPluralString
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
import Util


type MarketplaceFilter
    = MarketplaceFilter
        { whatToFilter : FilteredItem
        , ignoreWhen : Conditions
        , butNotWhen : Conditions
        }


emptyFilter : MarketplaceFilter
emptyFilter =
    MarketplaceFilter
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
marketplaceFilterValidationErrors (MarketplaceFilter mf) =
    let
        atLeastOnePositiveCondition =
            Util.validate (List.isEmpty <| conditionsToList mf.ignoreWhen) "Filtr musí obsahovat aspoň jednu podmínku"
    in
    atLeastOnePositiveCondition
        ++ conditionsValidationErrors "" mf.ignoreWhen
        ++ conditionsValidationErrors "Výjimka - " mf.butNotWhen


setFilteredItem : FilteredItem -> MarketplaceFilter -> MarketplaceFilter
setFilteredItem newItem (MarketplaceFilter mf) =
    MarketplaceFilter { mf | whatToFilter = newItem }


getFilteredItem : MarketplaceFilter -> FilteredItem
getFilteredItem (MarketplaceFilter { whatToFilter }) =
    whatToFilter


addPositiveCondition : Condition -> MarketplaceFilter -> MarketplaceFilter
addPositiveCondition c =
    updatePositiveConditions (addCondition c)


addNegativeCondition : Condition -> MarketplaceFilter -> MarketplaceFilter
addNegativeCondition c =
    updateNegativeConditions (addCondition c)


updatePositiveConditions : (Conditions -> Conditions) -> MarketplaceFilter -> MarketplaceFilter
updatePositiveConditions conditionsUpdater (MarketplaceFilter f) =
    MarketplaceFilter { f | ignoreWhen = conditionsUpdater f.ignoreWhen }


updateNegativeConditions : (Conditions -> Conditions) -> MarketplaceFilter -> MarketplaceFilter
updateNegativeConditions conditionsUpdater (MarketplaceFilter f) =
    MarketplaceFilter { f | butNotWhen = conditionsUpdater f.butNotWhen }


renderBuyFilter : MarketplaceFilter -> String
renderBuyFilter mf =
    "Ignorovat " ++ renderFilteredItem (getFilteredItem mf) ++ renderCommonPartOfBuyAndSellFilters mf


renderSellFilter : MarketplaceFilter -> String
renderSellFilter mf =
    "Prodat participaci" ++ renderCommonPartOfBuyAndSellFilters mf


renderCommonPartOfBuyAndSellFilters : MarketplaceFilter -> String
renderCommonPartOfBuyAndSellFilters (MarketplaceFilter { ignoreWhen, butNotWhen }) =
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
