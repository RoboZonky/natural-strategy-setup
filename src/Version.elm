module Version
    exposing
        ( commitHash
        , githubCommitLink
        , robozonkyVersionStatement
        , strategyComment
        )

import Time.Date as Date exposing (Date)


githubCommitLink : String
githubCommitLink =
    "https://github.com/RoboZonky/natural-strategy-setup/commit/" ++ commitHash


strategyComment : Date -> String
strategyComment today =
    "# Konfigurace strategie vytvořená " ++ formatDate today ++ " nástrojem natural-strategy-setup verze " ++ commitHash


robozonkyVersionStatement : String
robozonkyVersionStatement =
    "Tato strategie vyžaduje RoboZonky ve verzi 4.3.0 nebo pozdější."


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
