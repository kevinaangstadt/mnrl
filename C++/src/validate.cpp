#include <rapidjson/document.h>

#include <valijson/adapters/json11_adapter.hpp>
#include <valijson/utils/json11_utils.hpp>
#include <valijson/schema.hpp>
#include <valijson/schema_parser.hpp>
#include <valijson/validator.hpp>


using valijson::Schema;
using valijson::SchemaParser;
using valijson::Validator;
using valijson::adapters::Json11Adapter;

int main() {
    // Load JSON document using RapidJSON with Valijson helper function
    json11::Json mySchemaDoc;
    if (!valijson::utils::loadDocument("mnrl-schema.json", mySchemaDoc)) {
        throw std::runtime_error("Failed to load schema document");
    }
    
    // Parse JSON schema content using valijson
    Schema mySchema;
    SchemaParser parser;
    Json11Adapter mySchemaAdapter(mySchemaDoc);
    parser.populateSchema(mySchemaAdapter, mySchema);
    
    /*
    rapidjson::Document myTargetDoc;
    if (!valijson::utils::loadDocument("myTarget.json", myTargetDoc)) {
        throw std::runtime_error("Failed to load target document");
    }
    
    Validator validator;
    RapidJsonAdapter myTargetAdapter(myTargetDoc);
    if (!validator.validate(mySchema, myTargetAdapter, NULL)) {
        std::runtime_error("Validation failed.");
    }
    */
}