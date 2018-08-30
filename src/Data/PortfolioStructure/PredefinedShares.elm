module Data.PortfolioStructure.PredefinedShares
    exposing
        ( balanced
        , conservative
        , progressive
        )

import AllDict
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.PortfolioStructure exposing (PortfolioShares)


conservative : PortfolioShares
conservative =
    initShares
        [ ( A_Double_Star, 3 )
        , ( A_Star, 6 )
        , ( A_Double_Plus, 16 )
        , ( A_Plus, 25 )
        , ( A, 20 )
        , ( B, 15 )
        , ( C, 15 )
        , ( D, 0 )
        ]


balanced : PortfolioShares
balanced =
    initShares
        [ ( A_Double_Star, 1 )
        , ( A_Star, 3 )
        , ( A_Double_Plus, 17 )
        , ( A_Plus, 20 )
        , ( A, 25 )
        , ( B, 20 )
        , ( C, 12 )
        , ( D, 2 )
        ]


progressive : PortfolioShares
progressive =
    initShares
        [ ( A_Double_Star, 0 )
        , ( A_Star, 2 )
        , ( A_Double_Plus, 13 )
        , ( A_Plus, 15 )
        , ( A, 20 )
        , ( B, 25 )
        , ( C, 20 )
        , ( D, 5 )
        ]


initShares : List ( Rating, Int ) -> PortfolioShares
initShares =
    List.map (\( rtg, x ) -> ( rtg, Data.PortfolioStructure.percentageShare x x ))
        >> AllDict.fromList Rating.hash
