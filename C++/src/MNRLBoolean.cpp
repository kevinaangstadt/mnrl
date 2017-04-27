/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLUpCounter.cpp
 */

#include <json11.hpp>

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
	shared_ptr<MNRLReportId> reportId,
	shared_ptr<map<string,string>> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(portCount),
		gen_output(),
		attributes
),  mode(mode), reportId(reportId) {}

MNRLBoolean::MNRLBoolean(
	MNRLDefs::BooleanMode mode,
	int portCount,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	int reportId,
	shared_ptr<map<string,string>> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(portCount),
		gen_output(),
		attributes
),  mode(mode), reportId(shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(reportId))) {}

MNRLBoolean::MNRLBoolean(
	MNRLDefs::BooleanMode mode,
	int portCount,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	string reportId,
	shared_ptr<map<string,string>> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(portCount),
		gen_output(),
		attributes
),  mode(mode), reportId(shared_ptr<MNRLReportIdString>(new MNRLReportIdString(reportId))) {}

MNRLBoolean::MNRLBoolean(
	MNRLDefs::BooleanMode mode,
	int portCount,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	shared_ptr<map<string,string>> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(portCount),
		gen_output(),
		attributes
),  mode(mode), reportId(shared_ptr<MNRLReportId>(new MNRLReportId())) {}

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
	attrs.insert(map<string, Json>::value_type("reportId", Json(*reportId)));

	// insert latched
	attrs.insert(map<string, Json>::value_type("mode", Json(MNRLDefs::toMNRLBooleanMode(mode))));

	// update the attributes
	mapping["attributes"] = Json(attrs);

	return Json(mapping);
}

shared_ptr<MNRLReportId> MNRLBoolean::getReportId() { return reportId; }
void MNRLBoolean::setReportId(string id) { reportId = shared_ptr<MNRLReportIdString>(new MNRLReportIdString(id)); }
void MNRLBoolean::setReportId(int id) { reportId = shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(id)); }
void MNRLBoolean::setReportId(shared_ptr<MNRLReportId> id) { reportId = id; }
