module Data.PortfolioStructure.Predefined
    exposing
        ( balancedShares
        , conservativeShares
        , emptyShares
        , progressiveShares
        )

import AllDict
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.PortfolioStructure exposing (PortfolioShares)


conservativeShares : PortfolioShares
conservativeShares =
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


balancedShares : PortfolioShares
balancedShares =
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


progressiveShares : PortfolioShares
progressiveShares =
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


emptyShares : PortfolioShares
emptyShares =
    initShares
        [ ( A_Double_Star, 0 )
        , ( A_Star, 0 )
        , ( A_Double_Plus, 0 )
        , ( A_Plus, 0 )
        , ( A, 0 )
        , ( B, 0 )
        , ( C, 0 )
        , ( D, 0 )
        ]


initShares : List ( Rating, Int ) -> PortfolioShares
initShares =
    List.map (\( rtg, x ) -> ( rtg, Data.PortfolioStructure.percentageShare x x ))
        >> AllDict.fromList Rating.hash
