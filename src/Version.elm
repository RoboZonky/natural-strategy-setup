module Version
    exposing
        ( commitHash
        , githubCommitLink
        , strategyComment
        )


githubCommitLink : String
githubCommitLink =
    "https://github.com/RoboZonky/natural-strategy-setup/commit/" ++ commitHash


strategyComment : String
strategyComment =
    "# Konfigurace strategie vygenerovaná nástrojem natural-strategy-setup (verze " ++ commitHash ++ ")"


commitHash : String
commitHash =
    -- This placeholder is expected to be replaced by current commit number during the build
    "COMMIT_HASH_PLACEHOLDER"
