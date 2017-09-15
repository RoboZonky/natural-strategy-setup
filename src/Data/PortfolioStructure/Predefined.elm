module Data.PortfolioStructure.Predefined
    exposing
        ( balancedShares
        , conservativeShares
        , emptyShares
        , progressiveShares
        )

import AllDict
import Data.Filter.Condition.Rating as Rating exposing (Rating(..))
import Data.PortfolioStructure exposing (PortfolioShares)


conservativeShares : PortfolioShares
conservativeShares =
    AllDict.fromList Rating.hash
        [ ( A_Double_Star, ( 3, 3 ) )
        , ( A_Star, ( 6, 6 ) )
        , ( A_Double_Plus, ( 16, 16 ) )
        , ( A_Plus, ( 25, 25 ) )
        , ( A, ( 20, 20 ) )
        , ( B, ( 15, 15 ) )
        , ( C, ( 15, 15 ) )
        , ( D, ( 0, 0 ) )
        ]


balancedShares : PortfolioShares
balancedShares =
    AllDict.fromList Rating.hash
        [ ( A_Double_Star, ( 1, 1 ) )
        , ( A_Star, ( 3, 3 ) )
        , ( A_Double_Plus, ( 17, 17 ) )
        , ( A_Plus, ( 20, 20 ) )
        , ( A, ( 25, 25 ) )
        , ( B, ( 20, 20 ) )
        , ( C, ( 12, 12 ) )
        , ( D, ( 2, 2 ) )
        ]


progressiveShares : PortfolioShares
progressiveShares =
    AllDict.fromList Rating.hash
        [ ( A_Double_Star, ( 0, 0 ) )
        , ( A_Star, ( 2, 2 ) )
        , ( A_Double_Plus, ( 13, 13 ) )
        , ( A_Plus, ( 15, 15 ) )
        , ( A, ( 20, 20 ) )
        , ( B, ( 25, 25 ) )
        , ( C, ( 20, 20 ) )
        , ( D, ( 5, 5 ) )
        ]


emptyShares : PortfolioShares
emptyShares =
    AllDict.fromList Rating.hash
        [ ( A_Double_Star, ( 0, 0 ) )
        , ( A_Star, ( 0, 0 ) )
        , ( A_Double_Plus, ( 0, 0 ) )
        , ( A_Plus, ( 0, 0 ) )
        , ( A, ( 0, 0 ) )
        , ( B, ( 0, 0 ) )
        , ( C, ( 0, 0 ) )
        , ( D, ( 0, 0 ) )
        ]
