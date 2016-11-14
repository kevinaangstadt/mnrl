#!/usr/bin/python

import os, sys, argparse, json

sys.path.append(os.path.dirname(os.path.abspath(sys.argv[0])) + '/../')
sys.path.append(os.path.dirname(os.path.abspath(sys.argv[0])) + '/../untangle')
sys.path.append(os.path.dirname(os.path.abspath(sys.argv[0])) + '/../json-spec/src/')

from anml import *
import untangle
from jsonspec.validators import load

def raw2anml(raw):
    # our anml network
    anmlNet = AnmlNetwork(raw['id'])
    
    connections = []
    
    # add the STEs
    try:
        for ste in raw.state_transition_element:
            
            try:
                if ste.report_on_match is not None:
                    match = True
                    reportCode = ste.report_on_match['reportcode'] or 0
            except IndexError:
                match = False
                reportCode = 0
            
            anmlNet.AddSTE(
                ste['symbol-set'] or '',
                startType = ste['start'] or AnmlDefs.NO_START,
                anmlId = str(ste['id']),
                reportCode = reportCode,
                match = match,
                latched = ste['latched'] or False
            )
            
            try:
                for activate in ste.activate_on_match:
                    el = str(activate['element'])
                    if len(str.split(el,":")) == 1:
                        out = (el,AnmlDefs.STE_PORT)
                    else:
                        out = (str.split(el,":")[0], str.split(el,":")[1])
                    connections.append((str(ste['id']),out))
            except IndexError:
                pass
    except IndexError:
        pass
    
    # FIXME add the Boolean
    
    # add the Counters
    try:
        for counter in raw.counter:
            
            try:
                if counter.report_on_target is not None:
                    match = True
                    reportCode = counter.report_on_target['reportcode'] or 0
            except IndexError:
                match = False
                reportCode = 0
                
            anmlNet.AddCounter(
                   counter['target'] or 0,
                   mode = counter['at-target'],
                   anmlId = str(counter['id']),
                   reportCode = reportCode,
                   match = match,
                  )
            try:
                for activate in ste.activate_on_target:
                    el = str(activate['element'])
                    if len(str.split(el,":")) == 1:
                        out = (el,AnmlDefs.STE_PORT)
                    else:
                        out = (str.split(el,":")[0], str.split(el,":")[1])
                    connections.append((str(ste['id']),out))
            except IndexError:
                pass
            
    except IndexError:
        pass
            
    for (src,(dest,prt)) in connections:
        anmlNet.AddAnmlEdge(
                anmlNet.getElementByID(src),
                anmlNet.getElementByID(dest),
                destPort=prt
                )
    
    return anmlNet

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("anmlFile", help="the ANML file to convert")
    parser.add_argument("outfile", help="the destination of the MNRL file")
    args = parser.parse_args()
    
    # import the ANML file
    raw_anml = untangle.parse(args.anmlFile)
    
    try:
        network = raw2anml(raw_anml.anml.automata_network)
    except IndexError:
        try:
            network = raw2anml(raw_anml.automata_network)
        except IndexError:
            print "Could not find ANML root"
            sys.exit(1)
    
    f = open(os.path.dirname(os.path.abspath(sys.argv[0])) + '/../mnrl-schema.json')
    mnrl_schema = json.load(f)
    f.close()
    
    #mnrl = network.getMNRL()
    schema = load(mnrl_schema)
    
    schema.validate(json.loads(network.getMNRL()))
    
    #schema.validate(mnrl)
    
    f = open(args.outfile, 'w')
    f.write(json.dumps(json.loads(network.getMNRL()), indent=2, sort_keys=True))
    f.close()

if __name__ == '__main__':
    main()