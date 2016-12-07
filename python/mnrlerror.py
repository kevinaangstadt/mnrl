
class MNRLError(Exception):
    pass

class EnableError(MNRLError):
    def __init__(self, enable_code):
        super(EnableError, self).__init__("unknown enable code " + enable_code)
        self.enable_code = enable_code
        
class PortDefError(MNRLError):
    def __init__(self, ports):
        super(PortDefError, self).__init__(ports + "Def must be an array of tuples")

class PortIdError(MNRLError):
    def __init__(self, port_id):
        super(PortIdError, self).__init__("port_id is not a string")
        self.port_id = port_id
        
class DuplicateIdError(MNRLError):
    def __init__(self, id):
        super(DuplicateIdError, self).__init__(id + " is already defined")
        self.id = id

class DuplicatePortError(MNRLError):
    def __init__(self, port_id):
        super(DuplicatePortError, self).__init__(port_id + " is already defined")
        self.port_id = port_id

class InvalidStateOutputSymbols(MNRLError):
    def __init__(self):
        super(InvalidStateOutputSymbols, self).__init__("state symbols must be a tuple")

class UpCounterThresholdError(MNRLError):
    def __init__(self, threshold):
        super(UpCounterThresholdError, self).__init__("threshold must be a non-negative integer")
        self.threshold = threshold
        
class UpCounterModeError(MNRLError):
    def __init__(self, mode):
        super(UpCounterThresholdError, self).__init__("invalid mode")
        self.mode = mode
        
class InvalidGateFormat(MNRLError):
    def __init__(self):
        super(InvalidGateFormat, self).__init__("gate type must be a string")
        
class InvalidGateType(MNRLError):
    def __init__(self, gate_type):
        super(InvalidGateType, self).__init__("unknown gate type")
        self.gate_type = gate_type

class InvalidGatePortCount(MNRLError):
    def __init__(self, portCount):
        super(InvalidGatePortCount, self).__init__("gate port count must be a positive integer")
        self.portCount = portCount

class InvalidConnection(MNRLError):
    def __init__(self):
        super(InvalidConnection, self).__init__("connection endpoint must be a tuple")

class UnknownNode(MNRLError):
    def __init__(self,id):
        super(UnknownNode, self).__init__("node not found")
        self.id = id

class UnknownPort(MNRLError):
    def __init__(self,id,port_id):
        super(UnknownPort, self).__init__("port not found")
        self.id = id
        self.port_id = port_id
        
class PortWidthMismatch(MNRLError):
    def __init__(self,source,destination):
        super(PortWidthMismatch, self).__init__("port widths do not align")
        self.source = source
        self.destination = destination