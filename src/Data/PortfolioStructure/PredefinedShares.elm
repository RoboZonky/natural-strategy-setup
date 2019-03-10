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
        [ ( AAAAA, 3 )
        , ( AAAA, 6 )
        , ( AAA, 16 )
        , ( AA, 25 )
        , ( A, 20 )
        , ( B, 15 )
        , ( C, 15 )
        , ( D, 0 )
        ]


balanced : PortfolioShares
balanced =
    initShares
        [ ( AAAAA, 1 )
        , ( AAAA, 3 )
        , ( AAA, 17 )
        , ( AA, 20 )
        , ( A, 25 )
        , ( B, 20 )
        , ( C, 12 )
        , ( D, 2 )
        ]


progressive : PortfolioShares
progressive =
    initShares
        [ ( AAAAA, 0 )
        , ( AAAA, 2 )
        , ( AAA, 13 )
        , ( AA, 15 )
        , ( A, 20 )
        , ( B, 25 )
        , ( C, 20 )
        , ( D, 5 )
        ]


initShares : List ( Rating, Int ) -> PortfolioShares
initShares =
    List.map (\( rtg, x ) -> ( rtg, Data.PortfolioStructure.percentageShare x x ))
        >> Rating.initRatingDict
