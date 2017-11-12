module Version
    exposing
        ( commitHash
        , githubCommitLink
        , robozonkyVersionStatement
        , strategyComment
        )

import Time.DateTime as DateTime exposing (DateTime)


githubCommitLink : String
githubCommitLink =
    "https://github.com/RoboZonky/natural-strategy-setup/commit/" ++ commitHash


strategyComment : DateTime -> String
strategyComment dateTime =
    "# Konfigurace strategie vytvořená " ++ formatDate dateTime ++ " nástrojem natural-strategy-setup verze " ++ commitHash


robozonkyVersionStatement : String
robozonkyVersionStatement =
    "Tato strategie vyžaduje RoboZonky ve verzi 4.1.0 nebo pozdější."


formatDate : DateTime -> String
formatDate dateTime =
    let
        ( year, month, day, _, _, _, _ ) =
            DateTime.toTuple dateTime
    in
    toString day ++ "." ++ toString month ++ "." ++ toString year


commitHash : String
commitHash =
    -- This placeholder is expected to be replaced by current commit number during the build
    "COMMIT_HASH_PLACEHOLDER"
