module Data.Filter.Conditions.Story
    exposing
        ( Story(..)
        , StoryCondition(..)
        , StoryMsg
        , defaultStoryCondition
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


update : StoryMsg -> StoryCondition -> StoryCondition
update (SetStory s) _ =
    StoryCondition s


storyForm : StoryCondition -> Html StoryMsg
storyForm (StoryCondition currentStory) =
    div [] <| List.map (storyRadio currentStory) [ SHORT, BELOW_AVERAGE, AVERAGE, ABOVE_AVERAGE ]


storyRadio : Story -> Story -> Html StoryMsg
storyRadio currentStory thisRadiosStory =
    Radio.radio
        [ Radio.name "story"
        , Radio.checked <| currentStory == thisRadiosStory
        , Radio.onClick (SetStory thisRadiosStory)
        ]
        (storyToString thisRadiosStory)
