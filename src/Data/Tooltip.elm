module Data.Tooltip
    exposing
        ( States
        , TipId
        , confirmationTip
        , filterCreationTip
        , filterListTip
        , getState
        , getTooltipText
        , initialStates
        , portfolioStructureTip
        , update
        )

import Bootstrap.Popover as Popover
import Dict exposing (Dict)


type TipId
    = TipId Int


knownTooltips : List ( TipId, String )
knownTooltips =
    [ ( portfolioStructureTip, "Struktura portfolia definuje požadované rozložení zůstatkové částky do úvěrů na základě ratingu. Můžete zvolit jedno ze tří předdefinovaných portfolií (konzervativní, balancované, progresivní) nebo zvolte prázné a vyplňte požadované procentuální podíly sami." )
    , ( confirmationTip, "Úvěry s ratingem A, B, C a D jsou velmi žádané a v prvních minutách po uvedení na tržiště jsou chráněny CAPTCHA. Pokud pro ně nezapnete mobilní notifikace, robotovi se je s největší pravděpodobností nepodaří zainvestovat." )
    , ( filterListTip, "Filtry umožňují ignorovat některé úvěry (a participace) na tržišti. Daný úvěr (či participace) bude ignorován, pokud splní podmínky ALESPOŇ JEDNOHO jednoho z Vámi definovaných filtrů" )
    , ( filterCreationTip, "Filtr specifikuje jednu nebo více podmínek určujících, kdy má být úvěr (či participace) ignorován. Daný úvěr (či participace) bude ignorován, pokud splní VŠECHNY podmínky, které zde zvolíte.  Pokud však zároveň splní VŠECHNY podmínky výjimky, ignorován nebude." )
    ]


internalTipsDict : Dict Int String
internalTipsDict =
    Dict.fromList <| List.map (\( TipId x, tip ) -> ( x, tip )) knownTooltips


type States
    = States (Dict Int Popover.State)


initialStates : States
initialStates =
    States <| Dict.map (\_ _ -> Popover.initialState) internalTipsDict


getState : TipId -> States -> Popover.State
getState (TipId x) (States states) =
    Dict.get x states |> Maybe.withDefault Popover.initialState


update : TipId -> Popover.State -> States -> States
update (TipId x) s (States states) =
    States <| Dict.update x (Maybe.map (\_ -> s)) states


getTooltipText : TipId -> String
getTooltipText (TipId id) =
    Dict.get id internalTipsDict |> Maybe.withDefault "Nápověda není k dispozici"



-- Tooltip Ids


portfolioStructureTip : TipId
portfolioStructureTip =
    TipId 1


confirmationTip : TipId
confirmationTip =
    TipId 2


filterListTip : TipId
filterListTip =
    TipId 3


filterCreationTip : TipId
filterCreationTip =
    TipId 4
