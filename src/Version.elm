module Version
    exposing
        ( commitHash
        , filtersHowToLink
        , githubCommitLink
        , robozonkyVersionStatement
        , strategyComment
        )

import Html exposing (Html, a, text)
import Html.Attributes exposing (href)
import Time.Date as Date exposing (Date)


{-| Link to the document describing how filters should be configured.
-}
filtersHowToLink : Html a
filtersHowToLink =
    a [ href "https://github.com/RoboZonky/natural-strategy-setup/blob/master/docs/BuySellConfig.md" ]
        [ text "Nápověda" ]


githubCommitLink : String
githubCommitLink =
    "https://github.com/RoboZonky/natural-strategy-setup/commit/" ++ commitHash


strategyComment : Date -> String
strategyComment today =
    "# Konfigurace strategie vytvořená " ++ formatDate today ++ " nástrojem natural-strategy-setup verze " ++ commitHash


robozonkyVersionStatement : String
robozonkyVersionStatement =
    "Tato strategie vyžaduje RoboZonky ve verzi 4.5.0 nebo pozdější."


formatDate : Date -> String
formatDate date =
    let
        ( year, month, day ) =
            Date.toTuple date
    in
    toString day ++ "." ++ toString month ++ "." ++ toString year


commitHash : String
commitHash =
    -- This placeholder is expected to be replaced by current commit number during the build
    "COMMIT_HASH_PLACEHOLDER"
