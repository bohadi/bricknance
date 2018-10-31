{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , TemplateHaskell
#-}

module Frontend (
  runTui
) where

import Control.Monad.IO.Class (liftIO)
import Lens.Micro.TH (makeLenses)
import Lens.Micro    ((&), (%~), (.~), (^.))
import Data.Text

import qualified Brick as BR
import qualified Graphics.Vty as V

import Brick       ((<=>))
import Brick.BChan (BChan, writeBChan)

import Util

data TuiState = TuiState {
    _currentView :: CurrentView
  , _tickerView  :: Int
  , _bookView    :: Text
  , _endpoints   :: [StreamEndpoint]
}

data CurrentView =
  TickerView | BookViewData

data ResourceName = ResourceName {
} deriving (Eq, Ord)

makeLenses ''TuiState

initialState :: TuiState
initialState = TuiState {
    _currentView = TickerView
  , _tickerView  = 0
  , _bookView    = "world"
  , _endpoints   = defaultEndpoints
}

tui :: BR.App TuiState StreamEvent ResourceName
tui = BR.App {
    BR.appDraw         = redraw
  , BR.appChooseCursor = BR.neverShowCursor
  , BR.appHandleEvent  = update
  , BR.appStartEvent   = return
  , BR.appAttrMap      = attrMapper
}

-- TODO we're not receiving any payloads...
update :: TuiState -> BR.BrickEvent ResourceName StreamEvent -> BR.EventM ResourceName (BR.Next TuiState)
update tuiS (BR.AppEvent pl@MiniTickerPayload {..}) = BR.continue $ tuiS & tickerView %~ ((+) 1)
update tuiS (BR.AppEvent pl@AggTradePayload   {..}) = BR.continue tuiS
update tuiS (BR.AppEvent pl@TradePayload      {..}) = BR.continue $ tuiS & bookView %~ (cons 'b')
update tuiS (BR.AppEvent pl@TopDepthPayload   {..}) = BR.continue tuiS
update tuiS (BR.AppEvent pl@KlinePayload      {..}) = BR.continue tuiS
update tuiS (BR.VtyEvent (V.EvKey (V.KChar 's') [])) = BR.continue $ tuiS & bookView %~ (cons 's')
update tuiS (BR.VtyEvent (V.EvKey (V.KChar 'q') [])) = BR.halt tuiS
update tuiS (BR.VtyEvent (V.EvKey  V.KEsc       [])) = BR.halt tuiS
update tuiS _ = BR.continue tuiS

redraw :: TuiState -> [BR.Widget ResourceName]
redraw s = [
        (BR.str $ show $ s ^. tickerView)
    <=> (BR.txt $        s ^. bookView)
  ]

cursorChooser :: TuiState -> [BR.CursorLocation ResourceName] -> Maybe (BR.CursorLocation ResourceName)
cursorChooser s = undefined

attrMapper :: TuiState -> BR.AttrMap
attrMapper _ = BR.attrMap V.defAttr [
    ("tickerAttr", BR.bg V.blue)
  , ("bookAttr"  , BR.bg V.red)
  ]

runTui :: BChan StreamEvent -> BChan CmdEvent -> IO TuiState
runTui bfc _ = BR.customMain (V.mkVty V.defaultConfig) (Just bfc) tui initialState

