module Data.PortfolioStructure.PredefinedShares exposing
    ( balanced
    , conservative
    , progressive
    )

import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.PortfolioStructure exposing (PortfolioShares)


conservative : PortfolioShares
conservative =
    initShares
        [ ( AAAAA, 16 )
        , ( AAAA, 19 )
        , ( AAA, 21 )
        , ( AAE, 19 )
        , ( AA, 11 )
        , ( AE, 7 )
        , ( A, 5 )
        , ( B, 1.5 )
        , ( C, 0.5 )
        , ( D, 0 )
        ]


balanced : PortfolioShares
balanced =
    initShares
        [ ( AAAAA, 8 )
        , ( AAAA, 14 )
        , ( AAA, 16 )
        , ( AAE, 18 )
        , ( AA, 15 )
        , ( AE, 12 )
        , ( A, 9 )
        , ( B, 5 )
        , ( C, 2 )
        , ( D, 1 )
        ]


progressive : PortfolioShares
progressive =
    initShares
        [ ( AAAAA, 3 )
        , ( AAAA, 7 )
        , ( AAA, 10 )
        , ( AAE, 14 )
        , ( AA, 15 )
        , ( AE, 17 )
        , ( A, 15 )
        , ( B, 10 )
        , ( C, 6 )
        , ( D, 3 )
        ]


initShares : List ( Rating, Float ) -> PortfolioShares
initShares =
    List.map (\( rtg, x ) -> ( rtg, Data.PortfolioStructure.percentageShare x x ))
        >> Rating.initRatingDict
