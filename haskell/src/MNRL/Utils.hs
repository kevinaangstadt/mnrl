{-# LANGUAGE OverloadedStrings #-}

module MNRL.Utils where

import Control.Lens
import Data.Text (Text)
import qualified Control.Monad.State as St
import qualified Data.Map as M

import MNRL.Types

emptyMNRL :: MNRL
emptyMNRL = MNRL M.empty "__0__"

addComponent :: Id -> Component -> MNRL -> MNRL
addComponent i c m = m & mnrlComponents %~ (M.insert i c)

defaultNode :: Node
defaultNode  = Node { _nEnable = EOnActivateIn
                 , _nReport = False
                 , _nReportId = Just $ ReportInt 0
                 , _nReportEnable = Just RAlways
                 , _nInputDefs = []
                 , _nOutputDefs = []
                 }

mkHStateComp :: Text -> Component
mkHStateComp t = Component defaultNode (AttrHState $ HState False t)

mkCounterComp :: Int -> Component
mkCounterComp t = Component defaultNode (AttrCounter $ Counter CHigh t)

mkGate :: GateType -> Component
mkGate g = Component (defaultNode & nEnable .~ EOnStartAndActivateIn) (AttrGate $ Gate g)


-- addConnection :: Id -> PortId -> Id -> PortId -> MNRL -> MNRL
-- addConnection srcId srcPId destId destPId net
--     | net ^. srcId
-- addConnection srcId srcPId destId destPId net =
--     net & mnrlComponents %~ (modifySrc srcId srcPId)
--         & mnrlComponents %~ (modifyDest destId destPId)
--     where
--         modifySrc srcId srcPId compmap = undefined
--         modifyDest destId destPId compmap = undefined
