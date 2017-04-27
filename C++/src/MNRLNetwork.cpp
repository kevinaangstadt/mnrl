/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLNetwork.cpp
 */

#include <json11.hpp>
#include <json.hpp>

#include "MNRLNetwork.hpp"
#include "MNRLState.hpp"
#include "MNRLHState.hpp"
#include "MNRLUpCounter.hpp"
#include "MNRLBoolean.hpp"

using namespace std;
using namespace MNRL;
using namespace json11;

MNRLNetwork::MNRLNetwork(string id) : id(id) {
	nodes_added = 0;
}
MNRLNetwork::~MNRLNetwork() {}

Json MNRLNetwork::toJSON() {
	vector<Json> n;
	for(auto &kv : nodes) {
		n.push_back(kv.second->to_json());
	}

	return Json::object {
		{"id", id},
		{"nodes", n}
	};
}

void MNRLNetwork::exportToFile(string filename) {
	ofstream out(filename);
	out << nlohmann::json::parse(toJSON().dump()).dump(4) << endl;
	out.close();
}

MapIterator<map<string,shared_ptr<MNRLNode>>> MNRLNetwork::getNodeIterator() {
	return MapIterator<map<string,shared_ptr<MNRLNode>>>(nodes);
}

map<string,shared_ptr<MNRLNode>> MNRLNetwork::getNodes() {
	return nodes;
}

shared_ptr<MNRLNode> MNRLNetwork::getNodeById(string id){
	map<string,shared_ptr<MNRLNode>>::iterator it = nodes.find(id);

	if (it != nodes.end())
		return nodes[id];
	else
		throw MNRLError::UnknownNode(id);
}

shared_ptr<MNRLNode> MNRLNetwork::addNode(shared_ptr<MNRLNode> theNode) {
	string id = getUniqueNodeId(theNode->getId());
	theNode->setId(id);
	nodes.insert(map<string,shared_ptr<MNRLNode>>::value_type(theNode->getId(), theNode));
	return theNode;
}

shared_ptr<MNRLState> MNRLNetwork::addState(
	shared_ptr<vector<pair<string,string>>> outputSymbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	int reportId,
	bool latched,
	shared_ptr<map<string,string>> attributes
) {
	string new_id = getUniqueNodeId(id);
	shared_ptr<MNRLState> state = shared_ptr<MNRLState>(new MNRLState(outputSymbols, enable, new_id, report, reportId, latched, attributes));
	nodes.insert(map<string,shared_ptr<MNRLNode>>::value_type(new_id,state));
	return state;
}

shared_ptr<MNRLState> MNRLNetwork::addState(
	shared_ptr<vector<pair<string,string>>> outputSymbols,
	MNRLDefs::EnableType enable,
	std::string id,
	bool report,
	int reportId,
	bool latched
) {
	shared_ptr<map<string,string>> attr = shared_ptr<map<string,string>>(new map<string,string>());
	return addState(
		outputSymbols,
		enable,
		id,
		report,
		reportId,
		latched,
		attr
	);
}

shared_ptr<MNRLHState> MNRLNetwork::addHState(
	string symbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	int reportId,
	bool latched,
	shared_ptr<map<string,string>> attributes
) {
	string new_id = getUniqueNodeId(id);
	shared_ptr<MNRLHState> state = shared_ptr<MNRLHState>(new MNRLHState(symbols, enable, new_id, report, latched, reportId, attributes));
	nodes.insert(map<string,shared_ptr<MNRLNode>>::value_type(new_id,state));
	return state;
}

shared_ptr<MNRLHState> MNRLNetwork::addHState(
	string symbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	string reportId,
	bool latched,
	shared_ptr<map<string,string>> attributes
) {
	string new_id = getUniqueNodeId(id);
	shared_ptr<MNRLHState> state = shared_ptr<MNRLHState>(new MNRLHState(symbols, enable, new_id, report, latched, reportId, attributes));
	nodes.insert(map<string,shared_ptr<MNRLNode>>::value_type(new_id,state));
	return state;
}

shared_ptr<MNRLHState> MNRLNetwork::addHState(
	string symbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	bool latched,
	shared_ptr<map<string,string>> attributes
) {
	string new_id = getUniqueNodeId(id);
	shared_ptr<MNRLHState> state = shared_ptr<MNRLHState>(new MNRLHState(symbols, enable, new_id, report, latched, attributes));
	nodes.insert(map<string,shared_ptr<MNRLNode>>::value_type(new_id,state));
	return state;
}

shared_ptr<MNRLHState> MNRLNetwork::addHState(
	string symbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	int reportId,
	bool latched
){
	shared_ptr<map<string,string>> attr = shared_ptr<map<string,string>>(new map<string,string>());
	return addHState(
		symbols,
		enable,
		id,
		report,
		reportId,
		latched,
		attr
	);
}

shared_ptr<MNRLHState> MNRLNetwork::addHState(
	string symbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	string reportId,
	bool latched
){
	shared_ptr<map<string,string>> attr = shared_ptr<map<string,string>>(new map<string,string>());
	return addHState(
		symbols,
		enable,
		id,
		report,
		reportId,
		latched,
		attr
	);
}

shared_ptr<MNRLHState> MNRLNetwork::addHState(
	string symbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	bool latched
){
	shared_ptr<map<string,string>> attr = shared_ptr<map<string,string>>(new map<string,string>());
	return addHState(
		symbols,
		enable,
		id,
		report,
		latched,
		attr
	);
}

shared_ptr<MNRLUpCounter> MNRLNetwork::addUpCounter(
	int threshold,
	MNRLDefs::CounterMode mode,
	string id,
	bool report,
	int reportId,
	shared_ptr<map<string,string>> attributes
) {
	if(threshold < 0) {
		throw MNRLError::UpCounterThresholdError(threshold);
	}
	string new_id = getUniqueNodeId(id);
	shared_ptr<MNRLUpCounter> ctr = shared_ptr<MNRLUpCounter>(new MNRLUpCounter(threshold, mode, new_id, MNRLDefs::EnableType::ENABLE_ON_ACTIVATE_IN, report, reportId, attributes));
	nodes.insert(map<string,shared_ptr<MNRLNode>>::value_type(new_id,ctr));
	return ctr;
}

shared_ptr<MNRLUpCounter> MNRLNetwork::addUpCounter(
	int threshold,
	MNRLDefs::CounterMode mode,
	string id,
	bool report,
	int reportId
) {
	shared_ptr<map<string,string>> attr = shared_ptr<map<string,string>>(new map<string,string>());
	return addUpCounter(
		threshold,
		mode,
		id,
		report,
		reportId,
		attr
	);
}

shared_ptr<MNRLBoolean> MNRLNetwork::addBoolean(
	MNRLDefs::BooleanMode booleanType,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	int reportId,
	shared_ptr<map<string,string>> attributes
) {
	string new_id = getUniqueNodeId(id);
	shared_ptr<MNRLBoolean> b = shared_ptr<MNRLBoolean>(new MNRLBoolean(booleanType, MNRLDefs::BooleanToPort(booleanType), new_id, enable, report, reportId, attributes));
	nodes.insert(map<string,shared_ptr<MNRLNode>>::value_type(new_id,b));
	return b;
}

shared_ptr<MNRLBoolean> MNRLNetwork::addBoolean(
	MNRLDefs::BooleanMode booleanType,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	int reportId
) {
	shared_ptr<map<string,string>> attr = shared_ptr<map<string,string>>(new map<string,string>());
	return addBoolean(
		booleanType,
		enable,
		id,
		report,
		reportId,
		attr
	);
}

void MNRLNetwork::addConnection(string src, string src_port, string dest, string dest_port) {
	string s_id = src;
	shared_ptr<MNRLNode> s_node = getNodeById(src);
	shared_ptr<MNRLPort> s_port = s_node->getOutputPort(src_port);
	int s_output_width = s_port->getWidth();

	string d_id = dest;
	shared_ptr<MNRLNode> d_node = getNodeById(dest);
	shared_ptr<MNRLPort> d_port = d_node->getInputPort(dest_port);
	int d_input_width = d_port->getWidth();

	if(s_output_width != d_input_width)
		throw MNRLError::PortWidthMismatch(s_output_width, d_input_width);

	s_port->addConnection(d_node,d_port);
	d_port->addConnection(s_node,s_port);

}

void MNRLNetwork::removeConnection(string src, string src_port, string dest, string dest_port) {
	string s_id = src;
	shared_ptr<MNRLNode> s_node = getNodeById(s_id);
	shared_ptr<MNRLPort> s_port = s_node->getOutputPort(src_port);

	string d_id = dest;
	shared_ptr<MNRLNode> d_node = getNodeById(d_id);
	shared_ptr<MNRLPort> d_port = d_node->getInputPort(src_port);

	s_port->deleteConnection(d_id,dest_port);
	d_port->deleteConnection(s_id,src_port);
}

string MNRLNetwork::getUniqueNodeId(string id) {
	if(id.length() == 0) {
		string ret = "_" + to_string(nodes_added);
		nodes_added += 1;
		return ret;
	}

	map<string,shared_ptr<MNRLNode>>::iterator it = nodes.find(id);
	if(it != nodes.end())
		throw MNRLError::DuplicateIdError(id);

	return id;
}
