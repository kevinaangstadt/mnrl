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
    , "activate" .= toJSON (_oActivates out) ]

instance FromJSON OutputDef where
  parseJSON = withObject "OutputDef" $ \v -> OutputDef
    <$>  v .: "portId"
    <*>  v .: "width"
    <*>  v .: "activate"

instance FromJSON Component where
  parseJSON = withObject "Component" $ \v ->
    Component <$> (parseNode v) <*> (parseAttr v)

parseNode :: Object -> Parser Node
parseNode v = do
  nodeEnable <- (v .: "enable")
  nodeOuts <- (v .: "outputDefs")
  nodeIns <- (v .: "inputDefs")
  nodeReport <- v .:?  "report" .!= False
  nodeREnable <- v .:? "reportEnable"
  return $ Node nodeEnable nodeReport nodeREnable nodeIns nodeOuts

parseAttr :: Object -> Parser Attributes
parseAttr v = do
  nodeType  <- v .: "type" :: Parser Text
  case nodeType of
    "upCounter" -> AttrCounter <$> (parseCounter v)
    "boolean" -> AttrGate <$> (parseGate v)
    "hState" -> AttrHState <$> (parseHState v)
    "state" -> AttrState <$> (parseState v)
    _ -> fail "unclear node type"

parseHState :: Object -> Parser HState
parseHState v = do
  nodeType <- (v .: "type")
  guard $ nodeType == String "hState"
  attributes <- v .: "attributes"
  nodeLatched <- attributes .: "latched"
  nodeSymbols <- attributes .: "symbolSet"
  nodeRId <- attributes .: "reportId"
  return $ HState nodeLatched nodeSymbols nodeRId

parseState :: Object -> Parser State
parseState v = do
  nodeType <- (v .: "type")
  guard $ nodeType == String "state"
  attributes <- v .: "attributes"
  nodeLatched <- attributes .: "latched"
  nodeSymbols <- attributes .: "symbolSet"
  nodeRId <- attributes .: "reportId"
  return $ State nodeLatched nodeSymbols nodeRId

parseGate :: Object -> Parser Gate
parseGate v = do
  nodeType <- (v .: "type")
  guard $ nodeType == String "boolean"
  attributes <- v .: "attributes"
  gType <- attributes .: "gateType"
  gRId <- attributes .: "reportId"
  return $ Gate gType gRId

parseCounter :: Object -> Parser Counter
parseCounter v = do
  nodeType <- (v .: "type")
  guard $ nodeType == String "upCounter"
  attributes <- v .: "attributes"
  cMode <- attributes .: "mode"
  cThreshold <- attributes .: "threshold"
  cRId <- attributes .: "reportId"
  return $ Counter cMode cThreshold cRId

unparseHState :: HState -> Object
unparseHState hs = fromList [ "type" .= String "hState" ]
              <>   fromList [ "attributes" .=
                                object [ "latched" .= _hStateLatched hs
                                       , "symbolSet" .= _hStateSymbolSet hs
                                       , "reportId" .= _hStateReportId hs ] ]

unparseState :: State -> Object
unparseState s = fromList [ "type" .= String "state" ]
              <> fromList [ "attributes" .=
                              object [ "latched" .= _stateLatched s
                                     , "symbolSet" .= _stateSymbolSet s
                                     , "reportId" .= _stateReportId s ] ]

unparseGate :: Gate -> Object
unparseGate g = fromList [ "type" .= String "boolean" ]
             <> fromList [ "attributes" .=
                              object [ "gateType" .= _gateType g
                                     , "reportId" .= _gateReportId g ] ]

unparseCounter :: Counter -> Object
unparseCounter c = fromList [ "type" .= String "upCounter" ]
                <> fromList [ "attributes" .=
                              object [ "mode" .= _ctrMode c
                                     , "threshold" .= _ctrThreshold c
                                     , "reportId" .= _ctrReportId c ] ]

unparseAttrs :: Attributes -> Object
unparseAttrs (AttrHState x) = unparseHState x
unparseAttrs (AttrState x) = unparseState x
unparseAttrs (AttrCounter x) = unparseCounter x
unparseAttrs (AttrGate x) = unparseGate x

unparseNode :: Node -> Object
unparseNode node =
      (fromList [ "enable" .= _nEnable node
                , "report" .= _nReport node
                , "inputDefs" .= _nInputDefs node
                , "outputDefs" .= _nOutputDefs node ])
      <> (case (_nReportEnable node) of
            Nothing -> mempty
            Just x -> fromList [ "reportEnable" .= x])

unparseComponent :: Component -> Object
unparseComponent c = (unparseNode $ _cNode c) <> (unparseAttrs $ _cAttrs c)


instance ToJSON MNRL where
  toJSON n = object [
        "nodes" .= nodes
      , "id" .= _mnrlId n ]
    where
      comps = M.toList $ _mnrlComponents n
      nodes = map nodify comps
      nodify (i, n) = (fromList [ "id" .= String i ])
                      <> (unparseComponent n)

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
