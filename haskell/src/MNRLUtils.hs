{-# LANGUAGE OverloadedStrings #-}

module MNRLUtils where

import Control.Lens
import qualified Control.Monad.State as St
import qualified Data.Map as M

import MNRLTypes

emptyMNRL :: MNRL
emptyMNRL = MNRL M.empty "__0__"

addComponent :: Id -> Component -> MNRL -> MNRL
addComponent i c m = m & mnrlComponents %~ (M.insert i c)

-- addCounter :: Id -> Counter -> MNRL -> MNRL
-- addCounter = flip $ flip addComponent . AttrCounter
--
-- addState :: Id -> State -> MNRL -> MNRL
-- addState = flip $ flip addComponent . AttrState
--
-- addHState :: Id -> HState -> MNRL -> MNRL
-- addHState = flip $ flip addComponent . AttrHState
--
-- addGate :: Id -> Gate -> MNRL -> MNRL
-- addGate = flip $ flip addComponent . AttrGate

-- addConnection :: Id -> PortId -> Id -> PortId -> MNRL -> MNRL
-- addConnection srcId srcPId destId destPId net
--     | net ^. srcId
-- addConnection srcId srcPId destId destPId net =
--     net & mnrlComponents %~ (modifySrc srcId srcPId)
--         & mnrlComponents %~ (modifyDest destId destPId)
--     where
--         modifySrc srcId srcPId compmap = undefined
--         modifyDest destId destPId compmap = undefined

type BuilderState = Int

newId :: St.State BuilderState BuilderState
newId = St.get <* St.modify (+1)
