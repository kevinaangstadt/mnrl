/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * mnrl.cpp
 */

#include <json11.hpp>

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

MNRLReportId parseReportId(Json rid) {
	if(rid.is_number()) {
		return MNRLReportIdInt(rid.number_value());
	} else if (rid.is_string()) {
		return MNRLReportIdString(rid.string_value());
	} else
		return MNRLReportId();
}

shared_ptr<MNRLNode> parse_node(Json n) {
	string typ = n["type"].string_value();
	shared_ptr<MNRLNode> node;
	if( typ.compare("state") == 0 ) {
		// create output ports
		shared_ptr<vector<pair<string,string>>> output = shared_ptr<vector<pair<string,string>>>(new vector<pair<string,string>>());
		for(auto p : (n["attributes"]["symbolSet"]).object_items()) {
			output->push_back(make_pair(p.first,p.second.string_value()));
		}

		node = shared_ptr<MNRLNode>(new MNRLState(
				output,
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["id"].string_value(),
				n["report"].bool_value(),
				n["attributes"]["latched"].bool_value(),
				parseReportId(n["attributes"]["reportId"]),
				shared_ptr<Json::object>(new Json::object(n["attributes"].object_items()))
		));
	} else if( typ.compare("hState") == 0 ) {
		node = shared_ptr<MNRLNode>(new MNRLHState(
				n["attributes"]["symbolSet"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["id"].string_value(),
				n["report"].bool_value(),
				n["attributes"]["latched"].bool_value(),
				parseReportId(n["attributes"]["reportId"]),
				shared_ptr<Json::object>(new Json::object(n["attributes"].object_items()))
		));
	} else if( typ.compare("upCounter") == 0 ) {
		node = shared_ptr<MNRLNode>(new MNRLUpCounter(
				n["attributes"]["threshold"].int_value(),
				MNRLDefs::fromMNRLCounterMode(n["attributes"]["mode"].string_value()),
				n["id"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["report"].bool_value(),
				parseReportId(n["attributes"]["reportId"]),
				shared_ptr<Json::object>(new Json::object(n["attributes"].object_items()))
		));
	} else if( typ.compare("Boolean") == 0 ) {
		MNRLDefs::BooleanMode mode = MNRLDefs::fromMNRLBooleanMode(n["attributes"]["gateType"].string_value());
		node = shared_ptr<MNRLNode>(new MNRLBoolean(
				mode,
				MNRLDefs::BooleanToPort(mode),
				n["id"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["report"].bool_value(),
				parseReportId(n["attributes"]["reportId"]),
				shared_ptr<Json::object>(new Json::object(n["attributes"].object_items()))
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

		node = shared_ptr<MNRLNode>(new MNRLNode(
				n["id"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["report"].bool_value(),
				ins,
				outs,
				shared_ptr<Json::object>(new Json::object(n["attributes"].object_items()))
		));
	}
	return node;
}

/**
 * Helper function to read in the schema that's embedded in the library
 */
string MNRLSchema() {
	extern const char binary_mnrl_schema_json_start asm("_binary_mnrl_schema_json_start");
	extern const char binary_mnrl_schema_json_end asm("_binary_mnrl_schema_json_end");
	extern const int binary_mnrl_schema_json_size asm("_binary_mnrl_schema_json_size");

	string s;

	for(const char* i = &(binary_mnrl_schema_json_start); i<&(binary_mnrl_schema_json_end); ++i) {
		s.push_back(*i);
	}

	return s;
}

shared_ptr<MNRLNetwork> MNRL::loadMNRL(string filename) {
	// Load JSON schema using JSON11 with Valijson helper function
	string err;
	Json mySchemaDoc = Json::parse(MNRLSchema(),err);
	if(err.length() != 0) {
		throw std::runtime_error("Failed to load the MNRL Schema");
	}
//	if (!valijson::utils::loadDocument("mnrl-schema.json", mySchemaDoc)) {
//		throw std::runtime_error("Failed to load schema document");
//	}

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
	shared_ptr<MNRLNetwork> mnrl_obj = shared_ptr<MNRLNetwork>(new MNRLNetwork(mnrlDoc["id"].string_value()));

	/*
	 * Now build up the network in two steps
	 *
	 * 1. add all the nodes
	 * 2. add all the connections
	 */

	for(auto n : mnrlDoc["nodes"].array_items()) {
		shared_ptr<MNRLNode> node = parse_node(n);
		mnrl_obj->addNode(node);
	}

	for(auto n : mnrlDoc["nodes"].array_items()) {
		for(auto k : n["outputDefs"].array_items()) {
			for(auto c : k["activate"].array_items()) {
				mnrl_obj->addConnection(
						n["id"].string_value(),
						k["portId"].string_value(),
						c["id"].string_value(),
						c["portId"].string_value()
				);
			}
		}

	}

	return mnrl_obj;
}


