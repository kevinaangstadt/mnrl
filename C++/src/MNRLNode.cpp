/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLNode.cpp
 */


MNRL::MNRLNode(
		std::string id,
		MNRLDefs::EnableType enable,
		bool report,
		std::vector<MNRL::MNRLPort> inputDefs,
		std::vector<MNRL::MNRLPort> outputDefs,
		std::map<std::string, json11::Json> attributes
) : MNRL::MNRLNode::id(id),
    MNRL::MNRLNode::enable(enable),
	MNRL::MNRLNode::report(report),
	MNRL::MNRLNode::inputDefs( MNRL::MNRLNode::validate_ports(inputDefs) ),
	MNRL::MNRLNode::outputDefs( MNRL::MNRLNode::validate_ports(outputDefs) ),
	MNRL::MNRLNode::attributes(attributes) {};

MNRL::MNRLNode::~MNRLNode() {
	delete inputDefs;
	delete outputDefs;
};

json11::Json MNRL::MNRLNode::to_json() {

	// convert to input port definitions
	std::vector<json11::Json::object> iDefs;
	for(auto &kv : inputDefs) {
		iDefs.push_back(json11::Json::object {
			{ "portId", kv.first },
			{ "width", kv.second.getWidth() }
		});
	}

	// convert to output port definitions
	std::vector<json11::Json::object> oDefs;
	for(auto &kv : outputDefs) {
		// get all of the connections
		std::vector<std::pair<std::shared_ptr<MNRL::MNRLNode>, std::shared_ptr<MNRL::MNRLPort>>> conn = kv.second.getConnections();
		std::vector<json11::Json::object> mnrl_conn;
		for(std::pair<std::shared_ptr<MNRL::MNRLNode>, std::shared_ptr<MNRL::MNRLPort>> &np : conn) {
			mnrl_conn.push_back(json11::Json::object {
				{ "id", np.first->getId() },
				{ "portId", np.second->getId() }
			});
		}

		iDefs.push_back(json11::Json::object {
			{ "portId", kv.first },
			{ "width", kv.second.getWidth() },
			{ "activate", mnrl_conn }
		});
	}

	return json11::Json::object {
		{ "id", id },
		{ "report", report },
		{ "enable", MNRL::MNRLDefs::toMNRLEnable(enable) },
		{ "outputDefs", json11::Json::array(oDefs) },
		{ "inputDefs", json11::Json::array(iDefs) },
		{ "attributes", json11::Json::object(attributes) }
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

std::map<std::string, MNRL::MNRLPort> MNRL::MNRLNode::getOutputConnections(){ return outputDefs; }
std::map<std::string, MNRL::MNRLPort> MNRL::MNRLNode::getInputConnections(){ return inputDefs; }
std::string MNRL::MNRLNode::getId() { return id; }
bool MNRL::MNRLNode::getReport() { return report; }
MNRL::MNRLDefs::EnableType MNRL::MNRLNode::getEnable(){ return enable; }

void MNRL::MNRLNode::setId(std::string new_id) {
	id = new_id;
}

void MNRL::MNRLNode::setEnable(MNRL::MNRLDefs::EnableType e) {
	enable = e;
}

void MNRL::MNRLNode::setReport(bool b) {
	report = b;
}

std::map<std::string, MNRL::MNRLPort> MNRL::MNRLNode::validate_ports(std::vector<MNRL::MNRLPort> portdef) {
	std::map<std::string, MNRL::MNRLPort> *ports = new std::map<std::string, MNRL::MNRLPort>();

	for (MNRL::MNRLPort p : portdef) {
		ports->insert(std::map<std::string, MNRL::MNRLPort>::value_type(p.getId(),p));
	}

	return *ports;
}
