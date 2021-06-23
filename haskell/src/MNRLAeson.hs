{-# LANGUAGE OverloadedStrings #-}

module MNRLAeson where

import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Data.Text (Text)
import GHC.Exts
import Control.Monad
import qualified Data.Map as M

import MNRLTypes

toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"

instance ToJSON ReportId where
  toJSON (ReportInt i) = toJSON i
  toJSON (ReportString s) = toJSON s

instance FromJSON ReportId where
  parseJSON (String s) = return $ ReportString s
  parseJSON (Number i) = case toBoundedInteger i of
    Nothing -> fail "cannot decode report_id"
    Just i -> return (ReportInt i)
  parseJSON _ = fail "cannot decode report_id"

instance ToJSON CtrMode where
  toJSON CTrigger = toJSON ("trigger" :: Text)
  toJSON CHigh = toJSON ("high" :: Text)
  toJSON CRollOver = toJSON ("rollover" :: Text)

instance FromJSON CtrMode where
  parseJSON (String "trigger") = return CTrigger
  parseJSON (String "high") = return CHigh
  parseJSON (String "rollover") = return CRollOver
  parseJSON _ = fail "couldn't parse counter mode"

instance ToJSON GateType where
  toJSON GAnd = toJSON ("and" :: Text)
  toJSON GOr = toJSON ("or" :: Text)
  toJSON GNor = toJSON ("nor" :: Text)
  toJSON GNot = toJSON ("not" :: Text)
  toJSON GNand = toJSON ("nand" :: Text)

instance FromJSON GateType where
  parseJSON (String "and") = return GAnd
  parseJSON (String "or") = return GOr
  parseJSON (String "not") = return GNot
  parseJSON (String "nand") = return GNand
  parseJSON (String "nor") = return GNor
  parseJSON _ = fail "couldn't parse gate type"

instance ToJSON Enable where
  toJSON EOnActivateIn = toJSON ("onActivateIn" :: Text)
  toJSON EOnStartAndActivateIn = toJSON ("onStartAndActivateIn" :: Text)
  toJSON EOnLast = toJSON ("onLast" :: Text)
  toJSON EAlways = toJSON ("always" :: Text)

instance FromJSON Enable where
  parseJSON (String "onActivateIn") = return EOnActivateIn
  parseJSON (String "onStartAndActivateIn") = return EOnStartAndActivateIn
  parseJSON (String "onLast") = return EOnLast
  parseJSON (String "always") = return EAlways
  parseJSON _ = fail "couldn't parse enable"

instance ToJSON ReportEnable where
  toJSON ROnLast = toJSON ("onLast" :: Text)
  toJSON RAlways = toJSON ("always" :: Text)

instance FromJSON ReportEnable where
  parseJSON (String "onLast") = return ROnLast
  parseJSON (String "always") = return RAlways
  parseJSON _ = fail "couldn't parse report enable"

instance ToJSON InputDef where
  toJSON inp = object [
     "portId" .= _iPortId inp
   , "width"  .= _iWidth inp ]

instance FromJSON InputDef where
  parseJSON = withObject "InputDef" $ \v -> InputDef
    <$>  v .: "portId"
    <*>  v .: "width"

instance ToJSON Activate where
  toJSON act = object [
     "portId" .= _aPortId act
   , "id"  .= _aId act ]

instance FromJSON Activate where
  parseJSON = withObject "ActivateDef" $ \v -> Activate
    <$>  v .: "portId"
    <*>  v .: "id"

instance ToJSON OutputDef where
  toJSON out = object [
      "portId" .= _oPortId out
    , "width"  .= _oWidth out
    , "oActivates" .= toJSON (_oActivates out) ]

instance FromJSON OutputDef where
  parseJSON = withObject "OutputDef" $ \v -> OutputDef
    <$>  v .: "portId"
    <*>  v .: "width"
    <*>  v .: "activate"

instance ToJSON Node where
  toJSON node = Object $ (fromList [
      "enable" .= _nEnable node
    , "inputDefs" .= _nInputDefs node
    , "outputDefs" .= _nOutputDefs node ])
    <> (case (_nReportEnable node) of
        Nothing -> mempty
        Just x -> fromList [ "reportEnable" .= x])

instance ToJSON HState where
  toJSON hs = Object $
    toObject (toJSON (_hStateNode hs))
    <> fromList [ "type" .= String "hState" ]
    <> fromList [ "attributes" .= object [ "latched" .= _hStateLatched hs
                                           , "symbolSet" .= _hStateSymbolSet hs
                                           , "reportId" .= _hStateReportId hs ] ]

instance FromJSON HState where
  parseJSON = withObject "HState" $ \v -> do
    nodeType <- (v .: "type")
    guard $ nodeType == String "hState"
    nodeEnable <- (v .: "enable")
    nodeOuts <- (v .: "outputDefs")
    nodeIns <- (v .: "inputDefs")
    nodeReport <- v .:?  "report" .!= False
    nodeREnable <- v .:? "reportEnable"
    attributes <- v .: "attributes"
    nodeLatched <- attributes .: "latched"
    nodeSymbols <- attributes .: "symbolSet"
    nodeRId <- attributes .: "reportId"
    let n = Node nodeEnable nodeReport nodeREnable nodeIns nodeOuts
    let s = HState n nodeLatched nodeSymbols nodeRId
    return s

instance ToJSON State where
  toJSON s = Object $
    toObject (toJSON (_stateNode s))
    <> fromList [ "type" .= String "state" ]
    <> fromList [ "attributes" .= object [ "latched" .= _stateLatched s
                                           , "symbolSet" .= _stateSymbolSet s
                                           , "reportId" .= _stateReportId s ] ]

instance FromJSON State where
  parseJSON = withObject "State" $ \v -> do
    nodeType <- (v .: "type")
    guard $ nodeType == String "State"
    nodeEnable <- (v .: "enable")
    nodeOuts <- (v .: "outputDefs")
    nodeIns <- (v .: "inputDefs")
    nodeReport <- v .:?  "report" .!= False
    nodeREnable <- v .:? "reportEnable"
    attributes <- v .: "attributes"
    nodeLatched <- attributes .: "latched"
    nodeSymbols <- attributes .: "symbolSet"
    nodeRId <- attributes .: "reportId"
    let n = Node nodeEnable nodeReport nodeREnable nodeIns nodeOuts
    let s = State n nodeLatched nodeSymbols nodeRId
    return s

instance ToJSON Gate where
  toJSON g = Object $
    toObject (toJSON (_gateNode g))
    <> fromList [ "type" .= String "boolean" ]
    <> fromList [ "attributes" .= object [ "gateType" .= _gateType g
                                         , "reportId" .= _gateReportId g ] ]

instance FromJSON Gate where
  parseJSON = withObject "Gate" $ \v -> do
    nodeType <- (v .: "type")
    guard $ nodeType == String "boolean"
    nodeEnable <- (v .: "enable")
    nodeOuts <- (v .: "outputDefs")
    nodeIns <- (v .: "inputDefs")
    nodeReport <- v .:?  "report" .!= False
    nodeREnable <- v .:? "reportEnable"
    attributes <- v .: "attributes"
    gType <- attributes .: "gateType"
    gRId <- attributes .: "reportId"
    let n = Node nodeEnable nodeReport nodeREnable nodeIns nodeOuts
    let s = Gate n gType gRId
    return s

instance ToJSON Counter where
  toJSON c = Object $
    toObject (toJSON (_ctrNode c))
    <> fromList [ "type" .= String "upCounter" ]
    <> fromList [ "attributes" .= object [ "mode" .= _ctrMode c
                                         , "threshold" .= _ctrThreshold c
                                         , "reportId" .= _ctrReportId c ] ]

instance FromJSON Counter where
 parseJSON = withObject "Counter" $ \v -> do
   nodeType <- (v .: "type")
   guard $ nodeType == String "upCounter"
   nodeEnable <- (v .: "enable")
   nodeOuts <- (v .: "outputDefs")
   nodeIns <- (v .: "inputDefs")
   nodeReport <- v .:?  "report" .!= False
   nodeREnable <- v .:? "reportEnable"
   attributes <- v .: "attributes"
   cMode <- attributes .: "mode"
   cThreshold <- attributes .: "threshold"
   cRId <- attributes .: "reportId"
   let n = Node nodeEnable nodeReport nodeREnable nodeIns nodeOuts
   let s = Counter n cMode cThreshold cRId
   return s

instance ToJSON Component where
  toJSON (CompHState x) = toJSON x
  toJSON (CompState x) = toJSON x
  toJSON (CompCounter x) = toJSON x
  toJSON (CompGate x) = toJSON x

instance FromJSON Component where
  parseJSON = withObject "Component" $ \v -> do
    nodeType  <- v .: "type" :: Parser Text
    case nodeType of
      "upCounter" -> CompCounter <$> (parseJSON $ Object v)
      "boolean" -> CompGate <$> (parseJSON $ Object v)
      "hState" -> CompHState <$> (parseJSON $ Object v)
      "state" -> CompState <$> (parseJSON $ Object v)
      _ -> fail "unclear node type"

instance ToJSON MNRL where
  toJSON n = object [
        "nodes" .= nodes
      , "id" .= _mnrlId n ]
    where
      comps = M.toList $ _mnrlComponents n
      nodes = map nodify comps
      nodify (i, n) = (fromList [ "id" .= String i ])
                      <> (toObject $ toJSON n)

instance FromJSON MNRL where
  parseJSON = withObject "MNRL" $ \v -> do
    nodes <- v .: "nodes"
    MNRL <$> (mapify nodes) <*> (v .: "id")
    where
      mapify comps = M.fromList <$> (mapM f comps)
      f comp = do
        id <- comp .: "id"
        c <- parseJSON $ Object comp
        return $ (id, c)

