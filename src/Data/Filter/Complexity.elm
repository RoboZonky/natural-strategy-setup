module Data.Filter.Complexity exposing
    ( FilterComplexity(..)
    , complexityButtonLabel
    )

{-| To prevent issues with people not understanding propositional logic we distinguish two ways of creating filters:

Simple = 1. only one condition per filter allowed, 2. no rule exception allowed

Complex = everything allowed

-}


type FilterComplexity
    = Simple
    | Complex


complexityButtonLabel : FilterComplexity -> String
complexityButtonLabel filterComplexity =
    case filterComplexity of
        Simple ->
            "Přidat Pravidlo"

        Complex ->
            "Přidat Složité Pravidlo"
