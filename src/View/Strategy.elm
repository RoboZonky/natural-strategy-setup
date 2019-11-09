module View.Strategy exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration)
import Data.Tooltip as Tooltip
import Html exposing (Html, text)
import Time exposing (Posix)
import Types exposing (Msg(..))
import View.BuyingConfig as BuyingConfig
import View.CardHeightWorkaround exposing (markOpenedAccordionCard)
import View.ExitConfig as ExitConfig
import View.Filter.CreationModal as FilterCreationModal
import View.Filter.DeletionModal as FilterDeletionModal
import View.Investment as Investment
import View.InvestmentShare as InvestmentShare
import View.PortfolioStructure as PortfolioStructure
import View.ReservationSetting as ReservationSetting
import View.SellConfig as SellConfig
import View.TargetPortfolioSize as TargetPortfolioSize


form : StrategyConfiguration -> Accordion.State -> FilterCreationModal.Model -> FilterDeletionModal.Model -> Tooltip.States -> Posix -> Grid.Column Msg
form config accordionState filterCreationState filterDeletionState tooltipStates generatedOn =
    Grid.col
        [ Col.xs6 ]
        [ strategyForm config accordionState tooltipStates generatedOn
        , Html.map CreationModalMsg <| FilterCreationModal.view filterCreationState tooltipStates
        , FilterDeletionModal.view deletionModalConfig filterDeletionState
        ]


deletionModalConfig : FilterDeletionModal.Config Msg
deletionModalConfig =
    { setBuyingConfig = SetBuyingConfig
    , setSellingConfig = SetSellingConfig
    }


strategyForm : StrategyConfiguration -> Accordion.State -> Tooltip.States -> Posix -> Html Msg
strategyForm { generalSettings, portfolioStructure, investmentSizeOverrides, buyingConfig, sellingConfig } accordionState tooltipStates generatedOn =
    Accordion.config AccordionMsg
        |> Accordion.onlyOneOpen
        |> Accordion.cards
            [ generalSettingsCard generalSettings accordionState tooltipStates generatedOn
            , PortfolioStructure.form generalSettings.portfolio portfolioStructure accordionState tooltipStates
            , Investment.form generalSettings.defaultInvestmentSize investmentSizeOverrides
            , BuyingConfig.form buyingConfig accordionState tooltipStates
            , SellConfig.form sellingConfig accordionState tooltipStates
            ]
        |> Accordion.view accordionState


generalSettingsCard : GeneralSettings -> Accordion.State -> Tooltip.States -> Posix -> Accordion.Card Msg
generalSettingsCard settings accordionState tooltipStates generatedOn =
    let
        cardId =
            "generalSettingsCard"
    in
    Accordion.card
        { id = cardId
        , options = [ markOpenedAccordionCard cardId accordionState ]
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text "Obecná nastavení" ]
        , blocks =
            [ Accordion.block []
                [ TargetPortfolioSize.form settings.targetPortfolioSize
                , InvestmentShare.form settings.defaultInvestmentShare
                , ReservationSetting.form settings.reservationSetting
                , ExitConfig.form settings.exitConfig generatedOn tooltipStates
                ]
            ]
        }
