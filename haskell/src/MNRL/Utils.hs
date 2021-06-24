{-# LANGUAGE OverloadedStrings #-}

module MNRL.Utils where

import Control.Lens
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text (Text)
import Control.Arrow ((>>>))
import qualified Control.Monad.State as St
import qualified Data.Map as M

import MNRL.Types
import MNRL.Aeson

{--|
    This API mirrors the python implementation
    We have some default implementations for the most useful operations.
    You can use the lenses from MNRL.Types to modify each part individually.
    Note that it this API does not enforce the IDs to be unique.
    This is the programmer's responsibility.

    The Automaton exampleTwo constructs a simple automaton showing how to use this API.
--}

exampleTwo :: MNRL
exampleTwo = defaultMNRL
            &   (    addComponent "1" (mkHStateComp "a")
                 >>> addComponent "2" (mkHStateComp "b")
                 >>> addHS2HS "1" "2"
                )

instance Show MNRL where
    show = unpack . encodePretty

defaultMNRL :: MNRL
defaultMNRL = MNRL M.empty "__0__"

addComponent :: Id -> Component -> MNRL -> MNRL
addComponent i c m = m & mnrlComponents %~ (M.insert i c)

defaultNode :: Node
defaultNode  = Node { _nEnable = EOnActivateIn
                 , _nReport = False
                 , _nReportId = Just $ ReportInt 0
                 , _nReportEnable = Just RAlways
                 , _nInputDefs = stdInputs
                 , _nOutputDefs = stdOutputs
                 }

stdInputs :: [InputDef]
stdInputs = [InputDef "i" 1]

stdOutputs :: [OutputDef]
stdOutputs = [OutputDef "o" 1 []]

ctrInputs :: [InputDef]
ctrInputs = [InputDef "cnt" 1, InputDef "rst" 1]

mkHStateComp :: Text -> Component
mkHStateComp t = Component defaultNode (AttrHState $ HState False t)

mkCounterComp :: Int -> Component
mkCounterComp t = comp & cNode . nInputDefs .~ ctrInputs
    where 
        comp = Component defaultNode (AttrCounter $ Counter CHigh t)

mkGate :: GateType -> Component
mkGate g = Component (defaultNode & nEnable .~ EOnStartAndActivateIn) (AttrGate $ Gate g)


addConnection :: Id -> PortId -> Id -> PortId -> MNRL -> MNRL
addConnection srcId srcPId destId destPId net =
    net & mnrlComponents . at srcId . _Just . cNode . nOutputDefs
                         . traverse . (filtered pred) . oActivates %~ ((Activate destPId destId) :)
    where
        pred x = x ^. oPortId == srcPId

addHS2HS :: Id -> Id -> MNRL -> MNRL
addHS2HS srcId destId = addConnection srcId "o" destId "i"

