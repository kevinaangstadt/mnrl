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
using valijson::adapters::Json11Adapter;

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

	Json myTargetDoc;
	if (!valijson::utils::loadDocument(filename, myTargetDoc)) {
		throw std::runtime_error("Failed to load " + filename);
	}

	Validator validator;
	Json11Adapter myTargetAdapter(myTargetDoc);
	if (!validator.validate(mySchema, myTargetAdapter, NULL)) {
		std::runtime_error("Validation failed.");
	}

	// parse into MNRL
}
/*    with open("mnrl-schema.json", "r") as s:
        schema = json.load(s)
    with open(filename, "r") as f:
        json_string = f.read()

        try:
            jsonschema.validate(json.loads(json_string),schema)
        except jsonschema.exceptions.ValidationError as e:
            print "ERROR:", e
            return None

        # parse into MNRL
        d = MNRLDecoder()
        return d.decode(json_string)
        */
