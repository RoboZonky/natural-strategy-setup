module Data.Filter
    exposing
        ( BuyingConfiguration(..)
        , FilteredItem(..)
        , MarketplaceEnablement
        , MarketplaceFilter
        , SellingConfiguration(..)
        , addSellFilter
        , decodeBuyingConfiguration
        , decodeSellingConfiguration
        , emptyFilter
        , encodeBuyingConfiguration
        , encodeSellingConfiguration
        , filterTextView
        , getFiltersRemovedByBuyingConfigurationChange
        , isValid
        , itemToPluralString
        , marketplaceFilterValidationErrors
        , removeSellFilterAt
        , renderBuyingConfiguration
        , renderSellingConfiguration
        , setFilteredItem
        , togglePrimaryEnablement
        , toggleSecondaryEnablement
        , updateBuyFilters
        , updateNegativeConditions
        , updatePositiveConditions
        , validateSellingConfiguration
        )

import Bootstrap.Badge as Badge
import Data.Filter.Conditions as Conditions exposing (Condition, Conditions)
import Html exposing (Html, span, text)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Util


type BuyingConfiguration
    = InvestEverything
    | InvestSomething MarketplaceEnablement (List MarketplaceFilter)
    | InvestNothing


updateBuyFilters : (List MarketplaceFilter -> List MarketplaceFilter) -> BuyingConfiguration -> BuyingConfiguration
updateBuyFilters updater buyingConfiguration =
    case buyingConfiguration of
        InvestSomething enablement filters ->
            InvestSomething enablement <| updater filters

        InvestEverything ->
            InvestSomething { primaryEnabled = True, secondaryEnabled = True } (updater [])

        other ->
            other


addSellFilter : MarketplaceFilter -> SellingConfiguration -> SellingConfiguration
addSellFilter newFilter sellingConfiguration =
    case sellingConfiguration of
        SellNothing ->
            SellSomething [ newFilter ]

        SellSomething filters ->
            SellSomething <| filters ++ [ newFilter ]


removeSellFilterAt : Int -> SellingConfiguration -> SellingConfiguration
removeSellFilterAt index sellingConfiguration =
    case sellingConfiguration of
        SellNothing ->
            SellNothing

        SellSomething filters ->
            case List.Extra.removeAt index filters of
                [] ->
                    SellNothing

                nonEmptyFilterList ->
                    SellSomething nonEmptyFilterList


togglePrimaryEnablement : Bool -> BuyingConfiguration -> BuyingConfiguration
togglePrimaryEnablement enablePrimary buyingConfiguration =
    case buyingConfiguration of
        InvestEverything ->
            if enablePrimary then
                InvestEverything
            else
                InvestSomething { primaryEnabled = False, secondaryEnabled = True } []

        InvestSomething enablement filters ->
            let
                newEna =
                    { enablement | primaryEnabled = enablePrimary }
            in
            case ( newEna.primaryEnabled, newEna.secondaryEnabled ) of
                ( True, True ) ->
                    if List.isEmpty filters then
                        InvestEverything
                    else
                        InvestSomething newEna filters

                ( True, False ) ->
                    InvestSomething newEna filters

                ( False, True ) ->
                    InvestSomething newEna (removeDisabledFilters newEna filters)

                ( False, False ) ->
                    InvestNothing

        InvestNothing ->
            if enablePrimary then
                InvestSomething { primaryEnabled = True, secondaryEnabled = False } []
            else
                InvestNothing


toggleSecondaryEnablement : Bool -> BuyingConfiguration -> BuyingConfiguration
toggleSecondaryEnablement enableSecondary buyingConfiguration =
    case buyingConfiguration of
        InvestEverything ->
            if enableSecondary then
                InvestEverything
            else
                InvestSomething { primaryEnabled = True, secondaryEnabled = False } []

        InvestSomething enablement filters ->
            let
                newEna =
                    { enablement | secondaryEnabled = enableSecondary }
            in
            case ( newEna.primaryEnabled, newEna.secondaryEnabled ) of
                ( True, True ) ->
                    if List.isEmpty filters then
                        InvestEverything
                    else
                        InvestSomething newEna filters

                ( True, False ) ->
                    InvestSomething newEna (removeDisabledFilters newEna filters)

                ( False, True ) ->
                    InvestSomething newEna filters

                ( False, False ) ->
                    InvestNothing

        InvestNothing ->
            if enableSecondary then
                InvestSomething { primaryEnabled = False, secondaryEnabled = True } []
            else
                InvestNothing


{-| Calculate which filters would be removed during BuyingConfiguration change to decide if FilterDeletionModal should be displayed
-}
getFiltersRemovedByBuyingConfigurationChange : BuyingConfiguration -> BuyingConfiguration -> List MarketplaceFilter
getFiltersRemovedByBuyingConfigurationChange old new =
    case ( old, new ) of
        ( InvestNothing, _ ) ->
            []

        ( InvestEverything, _ ) ->
            []

        ( InvestSomething _ oldFilters, InvestNothing ) ->
            oldFilters

        ( InvestSomething _ oldFilters, InvestEverything ) ->
            oldFilters

        ( InvestSomething _ oldFilters, InvestSomething _ newFilters ) ->
            List.filter
                (\oldFilter -> not <| List.member oldFilter newFilters)
                oldFilters


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
            Util.validate (List.isEmpty filterList)
                "Seznam pravidel nesmí být prázdný. Přidejte alespoň jedno pravidlo nebo zakažte prodej participací"

        SellNothing ->
            []


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
    , ignoreWhen = Conditions.emptyConditions
    , butNotWhen = Conditions.emptyConditions
    }


renderBuyingConfiguration : BuyingConfiguration -> String
renderBuyingConfiguration buyingConfiguration =
    case buyingConfiguration of
        InvestEverything ->
            "Investovat do všech půjček a participací."

        InvestSomething enablement filters ->
            renderFilters "\n- Filtrování tržiště" enablement renderFilter filters

        InvestNothing ->
            "Ignorovat všechny půjčky i participace."


renderSellFilters : List MarketplaceFilter -> String
renderSellFilters filters =
    Util.renderNonemptySection "\n- Prodej participací" <|
        List.map renderFilter filters


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
    if isEnabled then
        "Investovat do všech půjček."
    else
        "Ignorovat všechny půjčky."


renderSecondaryEnablement : Bool -> String
renderSecondaryEnablement isEnabled =
    if isEnabled then
        "Investovat do všech participací."
    else
        "Ignorovat všechny participace."


isValid : MarketplaceFilter -> Bool
isValid =
    List.isEmpty << marketplaceFilterValidationErrors


marketplaceFilterValidationErrors : MarketplaceFilter -> List String
marketplaceFilterValidationErrors mf =
    let
        atLeastOnePositiveCondition =
            Util.validate (List.isEmpty <| Conditions.getEnabledConditions mf.ignoreWhen) "Pravidlo musí obsahovat aspoň jednu podmínku"
    in
    atLeastOnePositiveCondition
        ++ Conditions.conditionsValidationErrors "" mf.ignoreWhen
        ++ Conditions.conditionsValidationErrors "Výjimka - " mf.butNotWhen


setFilteredItem : FilteredItem -> MarketplaceFilter -> MarketplaceFilter
setFilteredItem newItem mf =
    { mf | whatToFilter = newItem }


updatePositiveConditions : (Conditions -> Conditions) -> MarketplaceFilter -> MarketplaceFilter
updatePositiveConditions conditionsUpdater mf =
    { mf | ignoreWhen = conditionsUpdater mf.ignoreWhen }


updateNegativeConditions : (Conditions -> Conditions) -> MarketplaceFilter -> MarketplaceFilter
updateNegativeConditions conditionsUpdater mf =
    { mf | butNotWhen = conditionsUpdater mf.butNotWhen }


filterPrefix : FilteredItem -> String
filterPrefix item =
    case item of
        Loan ->
            "Ignorovat úvěr"

        Participation ->
            "Ignorovat participaci"

        Loan_And_Participation ->
            "Ignorovat vše"

        Participation_To_Sell ->
            "Prodat participaci"


renderFilter : MarketplaceFilter -> String
renderFilter { whatToFilter, ignoreWhen, butNotWhen } =
    let
        prefix =
            filterPrefix whatToFilter ++ ", kde: "

        positivePart =
            renderConditionList <| Conditions.getEnabledConditions ignoreWhen

        negativePart =
            case Conditions.getEnabledConditions butNotWhen of
                [] ->
                    ""

                nonEmptyList ->
                    "\n(Ale ne když: " ++ renderConditionList nonEmptyList ++ ")"
    in
    prefix ++ positivePart ++ negativePart


{-| This renders condition text similar to renderFilter, but instead of joining
multiple conditions using ';' required by RoboZonky natural strategy parser,
it joins them by "a zároveň" highlighted in red to prevent issue reported in
<https://github.com/RoboZonky/natural-strategy-setup/issues/41>
-}
filterTextView : MarketplaceFilter -> Html a
filterTextView { whatToFilter, ignoreWhen, butNotWhen } =
    let
        prefix =
            Html.text <| filterPrefix whatToFilter ++ ", kde: "

        positivePart =
            renderConditionListWithExplicitConjunction <| Conditions.getEnabledConditions ignoreWhen

        negativePart =
            case Conditions.getEnabledConditions butNotWhen of
                [] ->
                    []

                nonEmptyList ->
                    Html.text "\n(Ale ne když: " :: renderConditionListWithExplicitConjunction nonEmptyList ++ [ Html.text ")" ]
    in
    Html.div [] <| prefix :: positivePart ++ negativePart


renderConditionList : List Condition -> String
renderConditionList =
    List.map Conditions.renderCondition
        >> String.join "; "
        >> addDotIfNotEmptyString


renderConditionListWithExplicitConjunction : List Condition -> List (Html a)
renderConditionListWithExplicitConjunction =
    List.map (Conditions.renderCondition >> Html.text)
        >> List.intersperse andConnective
        >> addDotIfNotEmptyList


andConnective : Html a
andConnective =
    span []
        [ text " "
        , Badge.pillInfo [] [ text "a zároveň" ]
        , text " "
        ]


addDotIfNotEmptyString : String -> String
addDotIfNotEmptyString s =
    s
        ++ (if String.isEmpty s then
                ""
            else
                "."
           )


addDotIfNotEmptyList : List (Html a) -> List (Html a)
addDotIfNotEmptyList xs =
    xs
        ++ (if List.isEmpty xs then
                []
            else
                [ Html.text "." ]
           )


type FilteredItem
    = Loan
    | Participation
    | Loan_And_Participation
    | Participation_To_Sell


allFilteredItems : List FilteredItem
allFilteredItems =
    [ Loan, Participation, Loan_And_Participation, Participation_To_Sell ]


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
                [ ( "m", Encode.int 0 )
                ]

        SellSomething filters ->
            Encode.object
                [ ( "m", Encode.int 1 )
                , ( "n", Encode.list <| List.map encodeMarketplaceFilter filters )
                ]


decodeSellingConfiguration : Decoder SellingConfiguration
decodeSellingConfiguration =
    Decode.field "m" Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    0 ->
                        Decode.succeed SellNothing

                    1 ->
                        Decode.map SellSomething
                            (Decode.field "n" (Decode.list marketplaceFilterDecoder))

                    _ ->
                        Decode.fail <| "Unable to decode SellingConfiguration from " ++ toString x
            )


encodeBuyingConfiguration : BuyingConfiguration -> Value
encodeBuyingConfiguration buyingConfiguration =
    case buyingConfiguration of
        InvestEverything ->
            Encode.object
                [ ( "o", Encode.int 0 )
                ]

        InvestSomething enablement filters ->
            Encode.object
                [ ( "o", Encode.int 1 )
                , ( "p", Encode.bool enablement.primaryEnabled )
                , ( "q", Encode.bool enablement.secondaryEnabled )
                , ( "r", Encode.list <| List.map encodeMarketplaceFilter filters )
                ]

        InvestNothing ->
            Encode.object
                [ ( "o", Encode.int 2 )
                ]


decodeBuyingConfiguration : Decoder BuyingConfiguration
decodeBuyingConfiguration =
    Decode.field "o" Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    0 ->
                        Decode.succeed InvestEverything

                    1 ->
                        Decode.map2 InvestSomething
                            (Decode.map2 MarketplaceEnablement
                                (Decode.field "p" Decode.bool)
                                (Decode.field "q" Decode.bool)
                            )
                            (Decode.field "r" (Decode.list marketplaceFilterDecoder))

                    2 ->
                        Decode.succeed InvestNothing

                    _ ->
                        Decode.fail <| "Unable to decode BuyingConfiguration from " ++ toString x
            )


encodeFilteredItem : FilteredItem -> Value
encodeFilteredItem =
    Util.enumEncoder allFilteredItems


encodeMarketplaceFilter : MarketplaceFilter -> Value
encodeMarketplaceFilter { whatToFilter, ignoreWhen, butNotWhen } =
    Encode.object
        [ ( "s", encodeFilteredItem whatToFilter )
        , ( "t", Conditions.encodeConditions ignoreWhen )
        , ( "u", Conditions.encodeConditions butNotWhen )
        ]


filteredItemDecoder : Decoder FilteredItem
filteredItemDecoder =
    Util.enumDecoder "FilteredItem" allFilteredItems


marketplaceFilterDecoder : Decoder MarketplaceFilter
marketplaceFilterDecoder =
    Decode.map3 MarketplaceFilter
        (Decode.field "s" filteredItemDecoder)
        (Decode.field "t" Conditions.conditionsDecoder)
        (Decode.field "u" Conditions.conditionsDecoder)
