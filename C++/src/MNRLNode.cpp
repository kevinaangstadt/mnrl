/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLNode.cpp
 */

#include "MNRLNode.hpp"

using namespace std;
using namespace MNRL;
using namespace json11;

MNRLNode::MNRLNode(
					string id,
					MNRLDefs::EnableType enable,
					bool report,
					vector<shared_ptr<MNRLPort>> inputDefs,
					vector<shared_ptr<MNRLPort>> outputDefs,
					map<string, shared_ptr<Json>> attributes
				) : id(id),
					enable(enable),
					report(report),
					inputDefs( MNRLNode::validate_ports(inputDefs) ),
					outputDefs( MNRLNode::validate_ports(outputDefs) ),
					attributes(attributes) {}

MNRLNode::~MNRLNode() {
	delete inputDefs;
	delete outputDefs;
}

Json MNRLNode::to_json() {

	// convert to input port definitions
	vector<Json::object> iDefs;
	for(auto &kv : *inputDefs) {
		iDefs.push_back(Json::object {
			{ "portId", kv.first },
			{ "width", kv.second.getWidth() }
		});
	}

	// convert to output port definitions
	vector<Json::object> oDefs;
	for(auto &kv : *outputDefs) {
		// get all of the connections
		port_conns conn = kv.second.getConnections();
		vector<Json::object> mnrl_conn;
		for(pair<shared_ptr<MNRLNode>, shared_ptr<MNRLPort>> &np : conn) {
			mnrl_conn.push_back(Json::object {
				{ "id", np.first->getId() },
				{ "portId", np.second->getId() }
			});
		}

		iDefs.push_back(Json::object {
			{ "portId", kv.first },
			{ "width", kv.second.getWidth() },
			{ "activate", mnrl_conn }
		});
	}

	return Json::object {
		{ "id", id },
		{ "report", report },
		{ "enable", MNRLDefs::toMNRLEnable(enable) },
		{ "outputDefs", Json(oDefs) },
		{ "inputDefs", Json(iDefs) },
		{ "attributes", Json(attributes) }
	};

	/*
	 * # define the enable string
        enable_string = MNRLDefs.toMNRLEnable(self.enable)

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
	 */
}

shared_ptr<port_map> MNRLNode::getOutputConnections(){ return outputDefs; }
shared_ptr<port_map> MNRLNode::getInputConnections(){ return inputDefs; }
string MNRLNode::getId() { return id; }
bool MNRLNode::getReport() { return report; }
MNRLDefs::EnableType MNRLNode::getEnable(){ return enable; }

void MNRLNode::setId(string new_id) {
	id = new_id;
}

void MNRLNode::setEnable(MNRLDefs::EnableType e) {
	enable = e;
}

void MNRLNode::setReport(bool b) {
	report = b;
}

shared_ptr<port_map> MNRLNode::validate_ports(vector<shared_ptr<MNRLPort>> &portdef) {
	shared_ptr<port_map> ports = shared_ptr<port_map>(new port_map());

	for (MNRLPort *p : portdef) {
		ports->insert(map<string, MNRLPort>::value_type(p->getId(),p));
	}

	return ports;
}
