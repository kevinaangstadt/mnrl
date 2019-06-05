/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * mnrl.cpp
 */

#include <set>
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

static map<string,string> getAttrs(const Json &jattr, set<string> exclude=set<string>()) {
	// get the items out of the map
	map<string,string> attrs;
	for(auto &a : jattr.object_items()) {
		if(exclude.find(a.first) != exclude.end() && a.second.is_string()) {
			string name = a.first;
			string value = a.second.string_value();
			attrs[name] = value;
		}
	}
	return attrs;
}

void parse_node(Json &n, MNRLNetwork &net) {
	string typ = n["type"].string_value();

	if( typ.compare("state") == 0 ) {
		// create output ports
		vector<pair<string,string>> output;
		for(auto p : (n["attributes"]["symbolSet"]).object_items()) {
			output.emplace_back(p.first,p.second.string_value());
		}

		set<string> excludes = {"symbolSet", "latched", "reportId"};

		n["attributes"]["reportId"].is_string() ?
		net.addState(
				output,
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["id"].string_value(),
				n["report"].bool_value(),
				n["attributes"]["reportId"].string_value(),
				n["attributes"]["latched"].bool_value(),
				getAttrs(n["attributes"], excludes)
		) :
		net.addState(
				output,
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["id"].string_value(),
				n["report"].bool_value(),
				n["attributes"]["reportId"].number_value(),
				n["attributes"]["latched"].bool_value(),
				getAttrs(n["attributes"], excludes)
		);
	} else if( typ.compare("hState") == 0 ) {
		set<string> excludes = {"symbolSet", "latched", "reportId"};
		n["attributes"]["reportId"].is_string() ?
		net.addHState(
				n["attributes"]["symbolSet"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["id"].string_value(),
				n["report"].bool_value(),
				n["attributes"]["reportId"].string_value(),
				n["attributes"]["latched"].bool_value(),
				getAttrs(n["attributes"], excludes)
		) :
		net.addHState(
				n["attributes"]["symbolSet"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["id"].string_value(),
				n["report"].bool_value(),
				n["attributes"]["reportId"].number_value(),
				n["attributes"]["latched"].bool_value(),
				getAttrs(n["attributes"], excludes)
		);
	} else if( typ.compare("upCounter") == 0 ) {
		set<string> excludes = {"mode", "threshold", "reportId"};
		n["attributes"]["reportId"].is_string() ?
		net.addUpCounter(
				n["attributes"]["threshold"].int_value(),
				MNRLDefs::fromMNRLCounterMode(n["attributes"]["mode"].string_value()),
				n["id"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["report"].bool_value(),
				n["attributes"]["reportId"].string_value(),
				getAttrs(n["attributes"], excludes)
		) :
		net.addUpCounter(
				n["attributes"]["threshold"].int_value(),
				MNRLDefs::fromMNRLCounterMode(n["attributes"]["mode"].string_value()),
				n["id"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["report"].bool_value(),
				n["attributes"]["reportId"].number_value(),
				getAttrs(n["attributes"], excludes)
		);
	} else if( typ.compare("boolean") == 0 ) {

		set<string> excludes = {"gateType", "reportId"};

		MNRLDefs::BooleanMode mode = MNRLDefs::fromMNRLBooleanMode(n["attributes"]["gateType"].string_value());
		n["attributes"]["reportId"].is_string() ?
		net.addBoolean(
				mode,
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["id"].string_value(),
				n["report"].bool_value(),
				n["attributes"]["reportId"].string_value(),
				getAttrs(n["attributes"], excludes)
		) : 
		net.addBoolean(
				mode,
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["id"].string_value(),
				n["report"].bool_value(),
				n["attributes"]["reportId"].number_value(),
				getAttrs(n["attributes"], excludes)
		);
	} else {
		// convert input defs into format needed for constructor
		port_def ins;
		for(auto k : n["inputDefs"].object_items()) {
			ins.emplace_back(k.second["portId"].string_value(), k.second["width"].int_value());
		}

		// convert output defs into format needed for constructor
		port_def outs;
		for(auto k : n["outputDefs"].object_items()) {
			outs.emplace_back(k.second["portId"].string_value(), k.second["width"].int_value());
		}

		net.addNode(
				n["id"].string_value(),
				MNRLDefs::fromMNRLEnable(n["enable"].string_value()),
				n["report"].bool_value(),
				ins,
				outs,
				getAttrs(n["attributes"])
		);
	}
	
	// set the reportEnable
	if(!n["reportEnable"].is_null())
		net.getNodeById(n["id"].string_value()).setReportEnable(MNRLDefs::fromMNRLReportEnable(n["reportEnable"].string_value()));

}

/**
 * Helper function to read in the schema that's embedded in the library
 */
string MNRLSchema() {
	extern const char binary_mnrl_schema_json_start asm("_binary_mnrl_schema_json_start");
	extern const char binary_mnrl_schema_json_end asm("_binary_mnrl_schema_json_end");
	extern const int binary_mnrl_schema_json_size asm("_binary_mnrl_schema_json_size");

	string s(&(binary_mnrl_schema_json_start), &(binary_mnrl_schema_json_end));

/*
	for(const char* i = &(binary_mnrl_schema_json_start); i<&(binary_mnrl_schema_json_end); ++i) {
		s.push_back(*i);
	}
	*/

	return s;
}

MNRLNetwork MNRL::loadMNRL(string &filename) {
	// Load JSON schema using JSON11 with Valijson helper function
	string err;
	Json mySchemaDoc = Json::parse(MNRLSchema(),err);
	if(err.length() != 0) {
		throw std::runtime_error("Failed to load the MNRL Schema");
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
	MNRLNetwork mnrl_obj(mnrlDoc["id"].string_value());

	/*
	 * Now build up the network in two steps
	 *
	 * 1. add all the nodes
	 * 2. add all the connections
	 */

	for(auto n : mnrlDoc["nodes"].array_items()) {
		parse_node(n, mnrl_obj);
	}

	for(auto n : mnrlDoc["nodes"].array_items()) {
		for(auto k : n["outputDefs"].array_items()) {
			for(auto c : k["activate"].array_items()) {
				mnrl_obj.addConnection(
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


