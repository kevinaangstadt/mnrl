{-# LANGUAGE OverloadedStrings #-}

module MNRLUtils where

import Control.Lens
import qualified Data.Map as M

import MNRLTypes

getNode :: Component -> Node
getNode (CompHState c) = _hStateNode c
getNode (CompState c) = _stateNode c
getNode (CompCounter c) = _ctrNode c
getNode (CompGate c) = _gateNode c

emptyMNRL :: MNRL
emptyMNRL = MNRL M.empty "__0__"

addComponent :: Id -> Component -> MNRL -> MNRL
addComponent i c m = m & mnrlComponents %~ (M.insert i c)

addCounter :: Id -> Counter -> MNRL -> MNRL
addCounter = flip $ flip addComponent . CompCounter

addState :: Id -> State -> MNRL -> MNRL
addState = flip $ flip addComponent . CompState

addHState :: Id -> HState -> MNRL -> MNRL
addHState = flip $ flip addComponent . CompHState

addGate :: Id -> Gate -> MNRL -> MNRL
addGate = flip $ flip addComponent . CompGate

-- addConnection :: Id -> PortId -> Id -> PortId -> MNRL -> MNRL
-- addConnection srcId srcPId destId destPId net
--     | net ^. srcId
-- addConnection srcId srcPId destId destPId net =
--     net & mnrlComponents %~ (modifySrc srcId srcPId)
--         & mnrlComponents %~ (modifyDest destId destPId)
--     where
--         modifySrc srcId srcPId compmap = undefined
--         modifyDest destId destPId compmap = undefined
