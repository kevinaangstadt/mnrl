/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLHState.cpp
 */

#include "MNRLHState.hpp"

using namespace std;
using namespace MNRL;
using namespace json11;

MNRLHState::MNRLHState(
	string symbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	bool latched,
	int reportId,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(),
		attributes
), symbols(symbols), reportId(reportId), latched(latched) {}

MNRLHState::~MNRLHState() {}

Json MNRLHState::to_json() {
	Json parent = MNRLNode::to_json();

	// we know that this is an obj
	map<string, Json> mapping = parent.object_items();

	// insert the type
	mapping.insert(map<string, Json>::value_type("type", Json("hState")));

	// get the attributes
	map<string, Json> attrs = mapping["attributes"].object_items();

	// insert the symbolSets
	attrs.insert(map<string, Json>::value_type("symbolSet", Json(symbols)));

	// insert reportId
	attrs.insert(map<string, Json>::value_type("reportId", Json(reportId)));

	// insert latched
	attrs.insert(map<string, Json>::value_type("latched", Json(latched)));

	// update the attributes
	mapping["attribute"] = Json(attrs);

	return Json(mapping);
}
