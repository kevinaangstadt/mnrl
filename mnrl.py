# Kevin Angstadt
# angstadt {at} virginia.edu
# University of Virginia
#
# Python objects for manipulating MNRL files

import json
import mnrlerror

class MNRLDefs(object):
    (ENABLE_ON_ACTIVATE_IN,
    ENABLE_ON_START_AND_ACTIVATE_IN,
    ENABLE_ALWAYS,
    ENABLE_ON_LAST,
    TRIGGER_ON_THRESHOLD,
    HIGH_ON_THRESHOLD,
    ROLLOVER_ON_THRESHOLD) = range(7)
    
    H_STATE_INPUT = STATE_INPUT = "i"
    H_STATE_OUTPUT = UP_COUNTER_OUTPUT = BOOLEAN_OUTPUT = "o"
    
    UP_COUNTER_COUNT = "cnt"
    UP_COUNTER_RESET = "rst"
    
    BOOLEAN_TYPES = {
        'and': 1,
        'or': 1,
        'nor': 1,
        'not': 1,
        'nand': 1
    }

class MNRLNetwork(object):
    """Represents the top level of a MNRL file."""
    
    def __init__(self, id):
        """Create a MNRL Network with an id set to 'id'"""
        self.id = id
        self.elements = dict()
        self._elements_added = 0
        
    def toJSON(self):
        return json.dumps({
            'id' : self.id,
            'elements' : [json.loads(e.toJSON()) for _,e in self.elements.iteritems()]
        })
    
    def getNodeById(self, id):
        """Return the element from the MNRL network with the given ID"""
        try:
            return self.elements[id]
        except KeyError:
            raise mnrlerror.UnknownNode(id)
    
    def addNode(self,theNode):
        """Add a MNRL Node object to the Network. Note that this may assign an
        ID to the node if none exists."""
        theNode.id = self._getUniqueNodeId(theNode.id)
        self.elements[theNode.id] = node
        return theNode
    
    def addState(self,
                 outputSymbols,
                 enable = MNRLDefs.ENABLE_ON_ACTIVATE_IN,
                 id = None,
                 report = False,
                 reportId = None,
                 latched = False,
                 attributes = {}
                ):
        """Create a state, add it to the network, and return it"""
        
        id = self._getUniqueNodeId(id)
        
        state = State(outputSymbols, enable=enable, id=id, report=report, reportId=reportId, latched=latched, attributes=attributes)
        
        self.elements[id] = state
        
        return state
    
    def addHState(self,
                  symbols,
                  enable = MNRLDefs.ENABLE_ON_ACTIVATE_IN,
                  id = None,
                  report = False,
                  reportId = None,
                  latched = False,
                  attributes = {}
                 ):
        """Create a homogenous state, add it to the network, and return it"""
        
        id = self._getUniqueNodeId(id)
        
        hState = HState(symbols,enable=enable,id=id,report=report,reportId=reportId,latched=latched,attributes=attributes)
        
        self.elements[id] = hState
        
        return hState
    
    def addUpCounter(self,
                     threshold,
                     mode = MNRLDefs.HIGH_ON_THRESHOLD,
                     id = None,
                     report = False,
                     reportId = None,
                     attributes = {}
                    ):
        """Create an up counter, add it to the network, and return it"""
        
        id = self._getUniqueNodeId(id)
        
        new_counter = UpCounter(target, mode=mode, id=id, report=report, reportId=reportId, attributes=attributes)
        
        self.elements[id] = new_counter
        
        return new_counter
    
    def addBoolean(self,
                   booleanType,
                   id = None,
                   report = False,
                   enable = MNRLDefs.ENABLE_ON_START_AND_ACTIVATE_IN,
                   reportId = None,
                   attributes = {}
                  ):
        """Create a Boolean node, add it to the network, and return it"""
        id = self._getUniqueNodeId(id)
        
        try:
            number_of_ports = MNRLDefs.BOOLEAN_TYPES[booleanType]
        except KeyError:
            raise mnrlerror.InvalidGateType(booleanType)
        
        boolean = Boolean(booleanType,portCount=number_of_ports,id=id,enable=enable,report=report,reportId=reportId,attributes=attributes)
        
        self.elements[id] = boolean
        
        return boolean
    
    def addConnection(self, source, destination):
        """Add a connection between node 'source' (id,port) and 'destination' (id,port)"""
        (s_id,
         s_port,
         s_node,
         s_output_width,
         s_output,
         d_id,
         d_port,
         d_node,
         d_input_width,
         d_input) = self.__getConnectionNodeInformation(source, destination)
        
        if s_output_width != d_input_width:
            raise mnrlerror.PortWidthMismatch(s_output_width, d_input_width)
        
        s_output.append({
            'id': d_id,
            'portId': d_port
        })
        
        d_input.append({
            'id': s_id,
            'portId': s_port
        })
    
    def removeConnection(self, source, destination):
        """Remove a connection between 'source' (id,port) and 'destination'
        (id,port). If no connection exists, do nothing."""
        (s_id,
         s_port,
         s_node,
         s_output_width,
         s_output,
         d_id,
         d_port,
         d_node,
         d_input_width,
         d_input) = self.__getConnectionNodeInformation(source, destination)
        
        # remove the connection
        try:
            s_output.remove({
                'id': d_id,
                'portId': d_port
            })
        except ValueError:
            pass # don't care
        
        try:
            d_input.remove({
                'id': s_id,
                'portId': s_port
            })
        except ValueError:
            pass # don't care
        
    def _getUniqueNodeId(self,id):
        """return a unique ID for the MNRL network. If an ID is passed in and is
        unique, it will be returned."""
        if id is None:
            id = "_" + self._elements_added
            self._elements_added += 1
            
        if id in self.elements:
            raise MNRLDuplicateId('This MNRL id already exists: ' + id)
        
        return id
    
    def __getConnectionNodeInformation(self, source, destination):
        try:
            s_id, s_port = source
            d_id, d_port = destination
        except ValueError:
            raise mnrlerror.InvalidConnection()
        
        s_node = self.getNodeById(s_id)
        d_node = self.getNodeById(d_id)
        
        try:
            s_output_width, s_output = s_node.outputDefs[s_port]
        except KeyError:
            raise mnrlerror.UnknownPort(s_id,s_port)
            
        try:
            d_input_width, d_input = d_node.inputDefs[d_port]
        except KeyError:
            raise mnrlerror.UnknownPort(d_id,d_port)
        
        return (s_id,
                s_port,
                s_node,
                s_output_width,
                s_output,
                d_id,
                d_port,
                d_node,
                d_input_width,
                d_input)

class MNRLNode(object):
    def __init__(self,
                 id = None,
                 enable = MNRLDefs.ENABLE_ON_ACTIVATE_IN,
                 report = False,
                 inputDefs = [],
                 outputDefs = [],
                 attributes = {}
                ):
        self.id = id
        
        if enable not in [
            MNRLDefs.ENABLE_ALWAYS,
            MNRLDefs.ENABLE_ON_ACTIVATE_IN,
            MNRLDefs.ENABLE_ON_START_AND_ACTIVATE_IN,
            MNRLDefs.ENABLE_ON_LAST
            ]:
            raise mnrlerror.EnableError(enable)
        self.enable = enable
        
        self.report = report
        
        #validate input ports
        self.inputDefs = self.__validate_ports(inputDefs,"input")
        
        #validate output ports
        self.outputDefs = self.__validate_ports(outputDefs,"output")
        
        self.attributes = attributes
    
    def toJSON(self):
        # define the enable string
        if self.enable == MNRLDefs.ENABLE_ON_ACTIVATE_IN:
            enable_string = "onActivateIn"
        elif self.enable == MNRLDefs.ENABLE_ON_START_AND_ACTIVATE_IN:
            enable_string = "onStartAndActivateIn"
        elif self.enable == MNRLDefs.ENABLE_ALWAYS:
            enable_string = "always"
        elif self.enable == MNRLDefs.ENABLE_ON_LAST:
            enable_string = "onLast"
            
        # properly define input ports (drop the connections)
        inputDefs = list()
        for port_id,(width,_) in self.inputDefs.iteritems():
            inputDefs.append({
                'portId': port_id,
                'width': width
            })
        
        # properly define output ports
        outputDefs = list()
        for port_id,(width,connection_list) in self.outputDefs.iteritems():
            outputDefs.append({
                'portId': port_id,
                'width': width,
                'activate': connection_list
            })
            
        return json.dumps({
            'id' : self.id,
            'report' : self.report,
            'enable' : enable_string,
            'inputDefs' : inputDefs,
            'outputDefs' : outputDefs,
            'attributes' : self.attributes
        })
    
    def getOutputConnections(self):
        """Returns the output connections dict of portid => (width, conn_list)"""
        return self.outputDefs
    
    def getInputConnections(self):
        """Returns the input connections dict of portid => (width, conn_list)"""
        return self.inputDefs
    
    def __validate_ports(self,port_def,inout):
        '''Returns a dictionary of ports. Keys are the port id's; each maps to a
        width and list of connections tuple.'''
        portDefs = dict()
        try:
            for port_id,width in port_def:
                # check that the port_id is a string
                if isinstance(port_id, basestring):
                    if port_id in portDefs:
                        raise mnrlerror.DuplicatePortId(port_id)
                    else:
                        if isinstance(width, int):
                            portDefs[port_id] = (width, [])
                        else:
                            raise mnrlerror.InvalidPortWidth(width)
                else:
                    raise mnrlerror.PortIdError(port_id, "the ID is not a string")
        except ValueError:
            raise mnrlerror.PortDefError(inout)
        return portDefs

class State(MNRLNode):
    """A state has one input port and multiple output ports. Output ports are
    enabled by seaparate symbol sets"""
    def __init__(self,
                 outputSymbols,
                 enable = MNRLDefs.ENABLE_ON_ACTIVATE_IN,
                 id = None,
                 report = False,
                 latched = False,
                 reportId = None,
                 attributes = {}
                ):
        
        stateAttributes = {
            'reportId': reportId,
            'latched': latched,
            'symbolSet': dict()
        }
        stateAttributes.update(attributes)
        
        # outputSymbols is a tuple:
        # ("outputId","symbolSet")
        outputDefs = []
        try:
            for output_id, symbol_set in outputSymbols:
                if isinstance(output_id, basestring):
                    stateAttributes['symbolSet'][output_id] = symbol_set
                    outputDefs.append((output_id,1))
                else:
                    raise mnrlerror.PortDefError("output")
        except ValueError:
            raise mnrlerror.InvalidStateOutputSymbols()
        
        super(State,self).__init__(
            id = id,
            enable = enable,
            report = report,
            inputDefs = [(MNRLDefs.H_STATE_INPUT,1)],
            outputDefs = outputDefs,
            attributes = stateAttributes
        )
    
    def toJSON(self):
        j = json.loads(super(State, self).toJSON())
        j.update({'type' : 'state'})
        return json.dumps(j)

class HState(MNRLNode):
    """Object representation of a homogeneous state. A homogenous state only has
    one input port and one output port."""
    def __init__(self,
                  symbols,
                  enable = MNRLDefs.ENABLE_ON_ACTIVATE_IN,
                  id = None,
                  report = False,
                  latched = False,
                  reportId = None,
                  attributes = {}
                ):
        
        hStateAttributes = {
            'latched': latched,
            'reportId': reportId,
            'symbolSet': symbols
        }
        hStateAttributes.update(attributes)
        
        super(HState, self).__init__(
            id = id,
            enable = enable,
            report = report,
            inputDefs = [(MNRLDefs.H_STATE_INPUT,1)],
            outputDefs = [(MNRLDefs.H_STATE_OUTPUT,1)],
            attributes = hStateAttributes
        )
    
    def toJSON(self):
        j = json.loads(super(HState, self).toJSON())
        j.update({'type' : 'hState'})
        return json.dumps(j)
        
class UpCounter(MNRLNode):
    def __init__(self,
                 threshold,
                 mode = MNRLDefs.HIGH_ON_THRESHOLD,
                 id = None,
                 report = False,
                 reportId = None,
                 attributes = {}
                ):
        
        counterAttributes = {
            'reportId': reportId,
            'threshold': threshold,
            'mode': mode
        }
        counterAttributes.update(attributes)
        
        #validate that the threshold is a non-negative int
        if not (isinstance(threshold, int) and threshold >= 0):
            raise mnrlerror.UpCounterThresholdError(threshold)
        
        #validate mode
        if mode not in [
            MNRLDefs.TRIGGER_ON_THRESHOLD,
            MNRLDefs.HIGH_ON_THRESHOLD,
            MNRLDefs.ROLLOVER_ON_THRESHOLD
        ]:
            raise mnrlerror.UpCounterModeError(mode)
        
        super(UpCounter,self).__init__(
            id = id,
            enable = MNRLDefs.ENABLE_ON_START_AND_ACTIVATE_IN, #a counter is always active
            report = report,
            inputDefs = [
                (MNRLDefs.UP_COUNTER_COUNT, 1),
                (MNRLDefs.UP_COUNTER_RESET, 1)
            ],
            outputDefs = [
                (MNRLDefs.UP_COUNTER_OUTPUT, 1)
            ],
            attributes = counterAttributes
        )
    
    def toJSON(self):
        j = json.loads(super(UpCounter, self).toJSON())
        j.update({'type' : 'upCounter'})
        return json.dumps(j)

class Boolean(MNRLNode):
    def __init__(self,
                 gateType,
                 portCount = 1,
                 id = None,
                 enable = MNRLDefs.ENABLE_ON_START_AND_ACTIVATE_IN,
                 report = False,
                 reportId = None,
                 attributes = {}
                ):
        
        if isinstance(gateType, basestring):
            if not (isinstance(portCount, int) and portCount > 0):
                raise mnrlerror.InvalidGatePortCount(portCount)
            
            # seems semi-valid, let's create it
            booleanAttributes = {
                'gateType': gateType,
                'reportId': reportId
            }
            booleanAttributes.update(attributes)
            
            inputDefs = []
            for i in range(portCount):
                inputDefs.append(("b" + str(i),1))
            
            super(Boolean, self).__init__(
                id = id,
                enable = enable,
                report = report,
                inputDefs = inputDefs,
                outputDefs = [(MNRLDefs.BOOLEAN_OUTPUT, 1)],
                attributes = booleanAttributes
            )
        else:
            raise mnrlerror.InvalidGateFormat()
    def toJSON(self):
        j = json.loads(super(Boolean, self).toJSON())
        j.update({'type' : 'boolean'})
        return json.dumps(j)