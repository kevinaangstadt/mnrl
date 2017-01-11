/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLUpCounter.cpp
 */

#include "MNRLBoolean.hpp"

using namespace std;
using namespace MNRL;
using namespace json11;

MNRLBoolean::MNRLBoolean(
	MNRLDefs::BooleanMode mode,
	int portCount,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	int reportId,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(portCount),
		gen_output(),
		attributes
),  mode(mode), reportId(MNRLReportIdInt(reportId)) {}

MNRLBoolean::MNRLBoolean(
	MNRLDefs::BooleanMode mode,
	int portCount,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	string reportId,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(portCount),
		gen_output(),
		attributes
),  mode(mode), reportId(MNRLReportIdString(reportId)) {}

MNRLBoolean::MNRLBoolean(
	MNRLDefs::BooleanMode mode,
	int portCount,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(portCount),
		gen_output(),
		attributes
),  mode(mode), reportId(MNRLReportId()) {}

MNRLBoolean::~MNRLBoolean() {}

Json MNRLBoolean::to_json() {
	Json parent = MNRLNode::to_json();

	// we know that this is an obj
	map<string, Json> mapping = parent.object_items();

	// insert the type
	mapping.insert(map<string, Json>::value_type("type", Json("boolean")));

	// get the attributes
	map<string, Json> attrs = mapping["attributes"].object_items();

	// insert reportId
	attrs.insert(map<string, Json>::value_type("reportId", Json(reportId)));

	// insert latched
	attrs.insert(map<string, Json>::value_type("mode", Json(MNRLDefs::toMNRLBooleanMode(mode))));

	// update the attributes
	mapping["attributes"] = Json(attrs);

	return Json(mapping);
}
