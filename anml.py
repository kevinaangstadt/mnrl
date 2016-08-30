# useful for manipulating ANML files

import json

class AnmlDefs(object):
    NO_START = "none"
    START_OF_DATA = "start-of-data"
    ALL_INPUT = "all-input"
    
    STE_PORT = "ste"
    T1_PORT = "t1"
    T2_PORT = "t2"
    T3_PORT = "t3"
    COUNT_ONE_PORT = "cnt"
    RESET_PORT = "rst"

class CounterMode:
    STOP_HOLD = "latch"
    ROLLOVER_PULSE = "roll"
    STOP_PULSE = "pulse"
    
class BooleanMode:
    AND = "and"
    OR = "or"
    SOP2 = "sum-of-products"
    POS2 = "product-of-sums"
    NAND = "nand"
    NOR = "nor"
    NSOP = "nsum-of-products"
    NPOS = "nproduct-of-sums"
    NOT = "inverter"

class AnmlNetwork(object):
    def __init__(self, id):
        self.id = id
        self.elements = dict()
    
    def __str__(self):
        anml = "<anml version=\"1.0\"  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">"
        anml += "\n<automata-network id=\"" + self.id + "\" >"
        
        for _,e in self.elements.iteritems():
            anml += "\n" + str(e)
        
        anml += "\n</automata-network>"
        anml += "\n</anml>"
        
        return anml
    
    def getMNRL(self):
        return json.dumps(
            {
                'id' : self.id,
                'elements' : [
                   json.loads(e.getMNRL()) for e in self.elements.values()
                ]
            }
        )
        
    def getElementByID(self, id):
        return self.elements[id]
    
    def AddSTE(self,
               symbols,
               startType = AnmlDefs.NO_START,
               anmlId = None,
               reportCode = 0,
               match = False,
               latched = False
              ):
        if anmlId in self.elements:
            raise AnmlDuplicateId('this anmlId already exists: ' + anmlId)
        
        new_ste = STE(symbols, startType=startType, anmlId=anmlId, reportCode=reportCode, match=match, latched=latched)
        
        self.elements[anmlId] = new_ste
        
        return new_ste
    
    def AddCounter(self,
                   target,
                   mode = CounterMode.STOP_HOLD,
                   anmlId = None,
                   reportCode = 0,
                   match = False,
                   cnt_used = 0
                  ):
        if anmlId in self.elements:
            raise AnmlDuplicateId('this anmlId already exists: ' + anmlId)
        
        new_counter = Counter(target, mode=mode, anmlId=anmlId, reportCode=reportCode, match=match, cnt_used=cnt_used)
        
        self.elements[anmlId] = new_counter
        
        return new_counter
    
    def AddBoolean(self,
                    mode=BooleanMode.AND,
                    eod=False,
                    anmlId=None,
                    terminals=1,
                    reportCode=0,
                    match=False
                   ):
        if anmlId in self.elements:
            raise AnmlDuplicateId('this anmlId already exists: ' + anmlId)
        
        new_boolean = Boolean(mode=mode, eod=eod, anmlId=anmlId, terminals=terminals, reportCode=reportCode, match=match)
        
        self.elements[anmlId] = new_boolean
        
        return new_boolean
    
    def AddAnmlEdge(self,
                    srcElement,
                    destElement,
                    destPort=AnmlDefs.STE_PORT
                    ):
        
        if srcElement.anmlId not in self.elements:
            raise AnmlElementNotFound('could not find source: ' + srcElement.anmlId)
        if destElement.anmlId not in self.elements:
            raise AnmlElementNotFound('could not find destination: ' + destElement.anmlId)
        
        srcElement.activate.append((destElement,destPort))
    
    def ExportAnml(self,
                   filename,
                   basename=None
                  ):
        f = open(filename, "w")
        f.write(str(self))
        f.close()
    

class STE(object):
    def __init__(self,
                 symbol,
                 startType=AnmlDefs.NO_START,
                 anmlId=None,
                 reportCode=0,
                 match=False,
                 latched=False
                ):
        self.symbol = symbol
        self.startType = startType
        self.anmlId = anmlId
        self.reportCode = reportCode
        self.match = match
        self.latched = latched
        self.activate = []
    
    def __str__(self):
        anml = "<state-transition-element id=\"" + self.anmlId + "\" symbol-set=\"" + self.symbol +"\" start=\"" + self.startType + "\" latch=\"" + str(self.latched).lower() + "\">"
        if self.match:
            anml += "\n    <report-on-match />"
        for e,p in self.activate:
            if p == AnmlDefs.STE_PORT or (p == AnmlDefs.T1_PORT and (e.mode == BooleanMode.AND or e.mode == BooleanMode.OR or e.mode == BooleanMode.NAND or e.mode == BooleanMode.NOR or e.mode == BooleanMode.NOT)):
                anml += "\n    <activate-on-match element=\"" + e.anmlId + "\" />"
            else:
                anml += "\n    <activate-on-match element=\"" + e.anmlId + ":" + p + "\" />"
        anml += "\n</state-transition-element>"
        return anml
    
    def getMNRL(self):
        MNRLObject = {
            'id': self.anmlId,
            'type': 'hState',
            'report': self.match,
            'inputDefs': [
                {
                    'portId': 'input',
                    'width': 1
                }
            ],
            'outputDefs': [
                {
                    'portId': 'output',
                    'activate': []
                }
            ],
            'attributes': {
                'symbolSet': self.symbol,
                'reportCode': self.reportCode,
                'latched': self.latched
            }
        }
        
        if self.startType == AnmlDefs.NO_START:
            MNRLObject['enable'] = 'onActivateIn'
        elif self.startType == AnmlDefs.ALL_INPUT:
            MNRLObject['enable'] = 'always'
        else:
            MNRLObject['enable'] = 'onStartAndActivateIn'
            
        for e,p in self.activate:
            if p == AnmlDefs.STE_PORT:
                MNRLObject['outputDefs'][0]['activate'].append({'id': e.anmlId, 'portId': 'input'})
            else:
                MNRLObject['outputDefs'][0]['activate'].append({'id': e.anmlId, 'portId': p})
                
        return json.dumps(MNRLObject)

class Counter(object):
    def __init__(self,
                 target,
                 mode=CounterMode.STOP_HOLD,
                 anmlId=None,
                 reportCode=0,
                 match=False,
                 cnt_used=0
                ):
        self.target = target
        self.mode = mode
        self.anmlId = anmlId
        self.reportCode = reportCode
        self.match = match
        self.cnt_used = cnt_used
        
        self.activate = []
    
    def __str__(self):
        anml = "<counter id=\"" + self.anmlId + "\" target=\"" + str(self.target) + "\" at-target=\"" + self.mode + "\" >"
        if self.match:
            anml += "\n     <report-on-target />"
        for e,p in self.activate:
            if p == AnmlDefs.STE_PORT or (p == AnmlDefs.T1_PORT and (e.mode == BooleanMode.AND or e.mode == BooleanMode.OR or e.mode == BooleanMode.NAND or e.mode == BooleanMode.NOR or e.mode == BooleanMode.NOT)):
                anml += "\n    <activate-on-target element=\"" + e.anmlId + "\" />"
            else:
                anml += "\n    <activate-on-target element=\"" + e.anmlId + ":" + p + "\" />"
        anml += "\n</counter>"
        
        return anml
    
    def getMNRL(self):
        MNRLObject = {
            'id': self.anmlId,
            'type': 'uCounter',
            'report': self.match,
            'enable': 'onActivateIn',
            'inputDefs': [
                {
                    'portId': AnmlDefs.COUNT_ONE_PORT,
                    'width': 1
                },
                {
                    'portId': AnmlDefs.RESET_PORT,
                    'width': 1
                }
            ],
            'outputDefs': [
                {
                    'portId': 'output',
                    'activate': []
                }
            ],
            'attributes': {
                'target': self.target,
                'reportCode': self.reportCode,
                'mode': self.mode
            }
        }
            
        for e,p in self.activate:
            if p == AnmlDefs.STE_PORT:
                MNRLObject['outputDefs'][0]['activate'].append({'id': e.anmlId, 'portId': 'input'})
            else:
                MNRLObject['outputDefs'][0]['activate'].append({'id': e.anmlId, 'portId': p})
                
        return json.dumps(MNRLObject)

class Boolean(object):
    def __init__(self,
                 mode=BooleanMode.AND,
                 eod=False,
                 anmlId=None,
                 terminals=1,
                 reportCode=0,
                 match=False
                ):
        self.mode = mode
        self.eod = eod
        self.anmlId = anmlId
        self.terminals = terminals
        self.reportCode = reportCode
        self.match = match
        
        self.activate = []
        
    def __str__(self):
        anml = "<" + self.mode + " id=\"" + self.anmlId + "\" high-only-on-eod=\"" + str(self.eod).lower() + "\" >"
        if self.match:
            anml += "\n     <report-on-high />"
        for e,p in self.activate:
            if p == AnmlDefs.STE_PORT or (p == AnmlDefs.T1_PORT and (e.mode == BooleanMode.AND or e.mode == BooleanMode.OR or e.mode == BooleanMode.NAND or e.mode == BooleanMode.NOR or e.mode == BooleanMode.NOT)):
                anml += "\n    <activate-on-high element=\"" + e.anmlId + "\" />"
            else:
                anml += "\n    <activate-on-high element=\"" + e.anmlId + ":" + p + "\" />"
        anml += "\n</" + self.mode + ">"
        
        return anml
    
    def getMNRL(self):
        MNRLObject = {
            'id': self.anmlId,
            'type': 'boolean',
            'report': self.match,
            'enable': 'always',
            'inputDefs': [],
            'outputDefs': [
                {
                    'portId': 'output',
                    'activate': []
                }
            ],
            'attributes': {
                'mode': self.mode,
                'eod': self.eod,
                'reportCode': self.reportCode,
                'mode': self.mode
            }
        }
        
        for i in range(self.terminals):
            MNRLObject['inputDefs'].append({'portId': 't'+str(i), 'width': 1})
            
        for e,p in self.activate:
            if p == AnmlDefs.STE_PORT:
                MNRLObject['outputDefs'][0]['activate'].append({'id': e.anmlId, 'portId': 'input'})
            else:
                MNRLObject['outputDefs'][0]['activate'].append({'id': e.anmlId, 'portId': p})
                
        return json.dumps(MNRLObject)

class AnmlDuplicateId(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

class AnmlElementNotFound(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)