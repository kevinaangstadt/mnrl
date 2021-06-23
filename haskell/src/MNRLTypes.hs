{-# LANGUAGE TemplateHaskell #-}

module MNRLTypes where

import Control.Lens
import Data.Map
import Data.Text (Text)

data MNRL = MNRL { _mnrlComponents :: [Component], _mnrlId :: Id }

data Component = CompHState HState | CompState State | CompCounter Counter | CompGate Gate

type Id = Text
data Enable = EOnActivateIn | EOnStartAndActivateIn | EOnLast | EAlways
type Report = Bool
data ReportEnable = RAlways | ROnLast
data InputDef = InputDef { _iPortId ::  Id, _iWidth :: Int }
data OutputDef = OutputDef { _oPortId ::  Id, _oWidth :: Int, _oActivates :: [Activate] }
data Activate = Activate { _aPortId :: Id, _aId :: Id}

data ReportId = ReportInt Int | ReportString Text

data CtrMode = CTrigger | CHigh | CRollOver

data GateType = GAnd | GOr | GNor | GNot | GNand

data Node = Node { _nId :: Id
            , _nEnable :: Enable
            , _nReport :: Report
            , _nReportEnable :: Maybe ReportEnable
            , _nInputDefs :: [InputDef]
            , _nOutputDefs :: [OutputDef]
            }

data HState = HState { _hStateNode :: Node
                     , _hStateLatched :: Bool
                     , _hStateSymbolSet :: Text
                     , _hStateReportId :: ReportId
                     }

data State = State { _stateNode :: Node
                   , _stateLatched :: Bool
                   , _stateSymbolSet :: Map Id Text
                   , _stateReportId :: ReportId
                   }

data Counter = Counter { _ctrNode :: Node
                       , _ctrMode :: CtrMode
                       , _ctrThreshold :: Int
                       , _ctrReportId :: ReportId
                       }

data Gate = Gate { _gateNode :: Node
                 , _gateType :: GateType
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
