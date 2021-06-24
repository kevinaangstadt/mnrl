{-# LANGUAGE TemplateHaskell #-}

module MNRLTypes where

import Control.Lens
import Data.Map
import Data.Text (Text)

data MNRL = MNRL { _mnrlComponents :: Map Id Component, _mnrlId :: Id }

data Component = Component { _cNode :: Node
                           , _cAttrs :: Attributes
                           }

data Attributes = AttrHState HState | AttrState State | AttrCounter Counter | AttrGate Gate

type Id = Text
type PortId = Text
data Enable = EOnActivateIn | EOnStartAndActivateIn | EOnLast | EAlways
type Report = Bool
data ReportEnable = RAlways | ROnLast
data InputDef = InputDef { _iPortId ::  PortId, _iWidth :: Int }
data OutputDef = OutputDef { _oPortId ::  PortId, _oWidth :: Int, _oActivates :: [Activate] }
data Activate = Activate { _aPortId :: PortId, _aId :: Id}

data ReportId = ReportInt Int | ReportString Text

data CtrMode = CTrigger | CHigh | CRollOver

data GateType = GAnd | GOr | GNor | GNot | GNand

data Node = Node { _nEnable :: Enable
                 , _nReport :: Report
                 , _nReportEnable :: Maybe ReportEnable
                 , _nInputDefs :: [InputDef]
                 , _nOutputDefs :: [OutputDef]
                 }

data HState = HState { _hStateLatched :: Bool
                     , _hStateSymbolSet :: Text
                     , _hStateReportId :: ReportId
                     }

data State = State { _stateLatched :: Bool
                   , _stateSymbolSet :: Map Id Text
                   , _stateReportId :: ReportId
                   }

data Counter = Counter { _ctrMode :: CtrMode
                       , _ctrThreshold :: Int
                       , _ctrReportId :: ReportId
                       }

data Gate = Gate { _gateType :: GateType
                 , _gateReportId :: ReportId}

makeLenses ''MNRL
makeLenses ''Component
makeLenses ''InputDef
makeLenses ''OutputDef
makeLenses ''Activate
makeLenses ''Node
makeLenses ''HState
makeLenses ''State
makeLenses ''Counter
makeLenses ''Gate
