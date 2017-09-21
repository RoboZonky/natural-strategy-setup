module Data.Filter
    exposing
        ( FilteredItem(..)
        , MarketplaceFilter(..)
        , addNegativeCondition
        , addPositiveCondition
        , emptyFilter
        , filtereedItemFromString
        , getFilteredItem
        , isValid
        , itemToPluralString
        , marketplaceFilterValidationErrors
        , renderFilteredItem
        , renderFilters
        , renderMarketplaceFilter
        , setFilteredItem
        , updateNegativeConditions
        , updatePositiveConditions
        )

import Data.Filter.Conditions exposing (..)
import Util


renderFilters : List MarketplaceFilter -> String
renderFilters filters =
    if List.isEmpty filters then
        ""
    else
        Util.joinNonemptyLines <| "\n- Filtrování tržiště" :: List.map renderMarketplaceFilter filters


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
addPositiveCondition c (MarketplaceFilter mf) =
    MarketplaceFilter { mf | ignoreWhen = addCondition c mf.ignoreWhen }


addNegativeCondition : Condition -> MarketplaceFilter -> MarketplaceFilter
addNegativeCondition c (MarketplaceFilter mf) =
    MarketplaceFilter { mf | butNotWhen = addCondition c mf.butNotWhen }


updatePositiveConditions : (Conditions -> Conditions) -> MarketplaceFilter -> MarketplaceFilter
updatePositiveConditions conditionsUpdater (MarketplaceFilter f) =
    MarketplaceFilter { f | ignoreWhen = conditionsUpdater f.ignoreWhen }


updateNegativeConditions : (Conditions -> Conditions) -> MarketplaceFilter -> MarketplaceFilter
updateNegativeConditions conditionsUpdater (MarketplaceFilter f) =
    MarketplaceFilter { f | butNotWhen = conditionsUpdater f.butNotWhen }


renderMarketplaceFilter : MarketplaceFilter -> String
renderMarketplaceFilter (MarketplaceFilter { whatToFilter, ignoreWhen, butNotWhen }) =
    let
        negativePart =
            if List.isEmpty (conditionsToList butNotWhen) then
                ""
            else
                "\n(Ale ne když: " ++ renderConditionList (conditionsToList butNotWhen) ++ ")"

        positivePart =
            renderConditionList <| conditionsToList ignoreWhen
    in
    "Ignorovat " ++ renderFilteredItem whatToFilter ++ ", kde: " ++ positivePart ++ negativePart


type FilteredItem
    = Loan
    | Participation
    | Loan_And_Participation


renderFilteredItem : FilteredItem -> String
renderFilteredItem item =
    case item of
        Loan_And_Participation ->
            "vše"

        Participation ->
            "participaci"

        Loan ->
            "úvěr"


itemToPluralString : FilteredItem -> String
itemToPluralString item =
    case item of
        Loan ->
            "Půjčky"

        Participation ->
            "Participace"

        Loan_And_Participation ->
            "Půjčky i participace"


filtereedItemFromString : String -> FilteredItem
filtereedItemFromString s =
    case s of
        "Loan_And_Participation" ->
            Loan_And_Participation

        "Participation" ->
            Participation

        _ ->
            Loan
