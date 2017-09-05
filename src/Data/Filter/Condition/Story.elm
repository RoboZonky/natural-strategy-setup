module Data.Filter.Condition.Story
    exposing
        ( Story(..)
        , StoryCondition(..)
        , StoryMsg
        , defaultStoryCondition
        , map
        , renderStoryCondition
        , storyForm
        , update
        )

import Bootstrap.Form.Radio as Radio
import Html exposing (Html, div)


type Story
    = SHORT
    | BELOW_AVERAGE
    | AVERAGE
    | ABOVE_AVERAGE


type StoryCondition
    = StoryCondition Story


defaultStoryCondition : StoryCondition
defaultStoryCondition =
    StoryCondition SHORT


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


type StoryMsg
    = SetStory Story


map : (Story -> Story) -> StoryCondition -> StoryCondition
map f (StoryCondition s) =
    StoryCondition (f s)


update : StoryMsg -> Story -> Story
update (SetStory s) _ =
    s


storyForm : Story -> Html StoryMsg
storyForm currentStory =
    div [] <| List.map (storyRadio currentStory) [ SHORT, BELOW_AVERAGE, AVERAGE, ABOVE_AVERAGE ]


storyRadio : Story -> Story -> Html StoryMsg
storyRadio currentStory thisRadiosStory =
    Radio.radio
        [ Radio.name "story"
        , Radio.checked <| currentStory == thisRadiosStory
        , Radio.onClick (SetStory thisRadiosStory)
        ]
        (storyToString thisRadiosStory)
