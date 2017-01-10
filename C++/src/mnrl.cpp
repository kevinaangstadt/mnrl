/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * mnrl.cpp
 */

#include <valijson/adapters/json11_adapter.hpp>
#include <valijson/utils/json11_utils.hpp>
#include <valijson/schema.hpp>
#include <valijson/schema_parser.hpp>
#include <valijson/validator.hpp>

#include "mnrl.hpp"

using namespace std;
using namespace MNRL;
using namespace json11;
using valijson::Schema;
using valijson::SchemaParser;
using valijson::Validator;
using valijson::ValidationResults;
using valijson::adapters::Json11Adapter;

shared_ptr<MNRLNode> parse_node(Json n) {
	string typ = n["type"].string_value();
	shared_ptr<MNRLNode> node;
	if( typ.compare("state") == 0 ) {
		// create output ports
		shared_ptr<vector<pair<string,string>>> output = new shared_ptr(new vector<pair<string,string>>());
		for(auto p : (n["attributes"]["symbolSet"]).object_items()) {
			output->push_back(p);
		}

		node = new shared_ptr(new MNRLState(
				output,
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["id"].string_value(),
				n["report"].bool_value(),
				n["attributes"]["latched"].bool_value(),
				n["attributes"]["reportId"].bool_value(),
				new shared_ptr<Json::object>(n["attributes"])
		));
	} else if( typ.compare("hState") == 0 ) {
		node = new shared_ptr(new MNRLHState(
				n["attributes"]["symbolSet"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["id"].string_value(),
				n["report"].bool_value(),
				n["attributes"]["latched"].bool_value(),
				n["attributes"]["reportId"].bool_value(),
				new shared_ptr<Json::object>(n["attributes"])
		));
	} else if( typ.compare("upCounter") == 0 ) {
		node = new shared_ptr(new MNRLUpCounter(
				n["attributes"]["threshold"].int_value(),
				MNRLDefs::fromMNRLCounterMode(n["attributes"]["mode"].string_value()),
				n["id"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["report"].bool_value(),
				n["attributes"]["reportId"].bool_value(),
				new shared_ptr<Json::object>(n["attributes"])
		));
	} else if( typ.compare("Boolean") == 0 ) {
		MNRLDefs::BooleanMode mode = MNRLDefs::fromMNRLBooleanMode(n["attribute"]["gateType"].string_value());
		node = new shared_ptr(new MNRLBoolean(
				mode,
				MNRLDefs::BooleanToPort(mode),
				n["id"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["report"].bool_value(),
				n["attributes"]["reportId"].bool_value(),
				new shared_ptr<Json::object>(n["attributes"])
		));
	} else {
		// convert input defs into format needed for constructor
		port_def ins;
		for(auto k : n["inputDefs"].object_items()) {
			ins.push_back(shared_ptr<MNRLPort>(new MNRLPort(k.second["portId"].string_value(), k.second["width"].int_value())));
		}

		// convert output defs into format needed for constructor
		port_def outs;
		for(auto k : n["outputDefs"].object_items()) {
			outs.push_back(shared_ptr<MNRLPort>(new MNRLPort(k.second["portId"].string_value(), k.second["width"].int_value())));
		}

		node = new shared_ptr(new MNRLNode(
				n["id"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["report"].bool_value(),
				ins,
				outs,
				new shared_ptr<Json::object>(n["attributes"])
		));
	}
	return node;
}

shared_ptr<MNRLNetwork> loadMNRL(string filename) {
	// Load JSON schema using JSON11 with Valijson helper function
	Json mySchemaDoc;
	if (!valijson::utils::loadDocument("mnrl-schema.json", mySchemaDoc)) {
		throw std::runtime_error("Failed to load schema document");
	}

	// Parse JSON schema content using valijson
	Schema mySchema;
	SchemaParser parser;
	Json11Adapter mySchemaAdapter(mySchemaDoc);
	parser.populateSchema(mySchemaAdapter, mySchema);

	Json mnrlDoc;
	if (!valijson::utils::loadDocument(filename, mnrlDoc)) {
		throw std::runtime_error("Failed to load " + filename);
	}

	Validator validator;
	ValidationResults results;
	Json11Adapter myTargetAdapter(mnrlDoc);
	if (!validator.validate(mySchema, myTargetAdapter, &results)) {
		for(auto e : results) {
			cout << e.description << endl;
		}
		std::runtime_error("Validation failed.");
	}

	// parse into MNRL
	// create the MNRLNetwork object, including the ID
	shared_ptr<MNRLNetwork> mnrl_obj = new shared_ptr<MNRLNetwork>(new MNRLNetwork(mnrlDoc["id"].string_value()));

	/*
	 * Now build up the network in two steps
	 *
	 * 1. add all the nodes
	 * 2. add all the connections
	 */

	for(auto n : mnrlDoc["nodes"].object_items()) {
		shared_ptr<MNRLNode> node = parse_node(n.second);
		mnrl_obj->addNode(node);
	}

	for(auto n : mnrlDoc["nodes"].object_items()) {
		for(auto k : n.second["outputDefs"].object_items()) {
			for(auto c : k.second["activate"].object_items()) {
				mnrl_obj->addConnection(
						n.second["id"].string_value(),
						k.second["portId"].string_value(),
						c.second["portId"].string_value(),
						c.second["portId"].string_value()
				);
			}
		}

	}

	return mnrl_obj;
}


