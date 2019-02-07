module View.CardHeightWorkaround exposing (markOpenedAccordionCard)

import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Html.Attributes exposing (class)


{-| 1st of 2 parts of the workaround for <https://github.com/RoboZonky/natural-strategy-setup/issues/8>.
Only needs to be applied to places where card content height can change dynamically
-}
markOpenedAccordionCard : String -> Accordion.State -> Card.Option a
markOpenedAccordionCard cardId accordionState =
    Card.attrs <|
        if Accordion.isOpen cardId accordionState then
            [ class "card-open" ]

        else
            []
