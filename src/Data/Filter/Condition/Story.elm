module Data.Filter.Condition.Story
    exposing
        ( Story(..)
        , StoryCondition(..)
        , renderStoryCondition
        )


type Story
    = SHORT
    | BELOW_AVERAGE
    | AVERAGE
    | ABOVE_AVERAGE


type StoryCondition
    = StoryCondition Story


storyToString : Story -> String
storyToString story =
    case story of
        SHORT ->
            "velmi krátký"

        BELOW_AVERAGE ->
            "kratší než průměrný"

        AVERAGE ->
            "průměrně dlouhý"

        ABOVE_AVERAGE ->
            "delší než průměrný"


renderStoryCondition : StoryCondition -> String
renderStoryCondition (StoryCondition story) =
    "příběh je " ++ storyToString story
