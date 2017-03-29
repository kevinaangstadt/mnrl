/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLState.cpp
 */

#include "MNRLState.hpp"

using namespace std;
using namespace MNRL;
using namespace json11;

MNRLState::MNRLState(
	shared_ptr<vector<pair<string,string>>> outputSymbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	bool latched,
	MNRLReportId reportId,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(outputSymbols),
		attributes
), outputSymbols(outputSymbols), reportId(reportId), latched(latched) {}

MNRLState::MNRLState(
	shared_ptr<vector<pair<string,string>>> outputSymbols,
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
		gen_output(outputSymbols),
		attributes
), outputSymbols(outputSymbols), reportId(MNRLReportIdInt(reportId)), latched(latched) {}

MNRLState::MNRLState(
	shared_ptr<vector<pair<string,string>>> outputSymbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	bool latched,
	string reportId,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(outputSymbols),
		attributes
), outputSymbols(outputSymbols), reportId(MNRLReportIdString(reportId)), latched(latched) {}

MNRLState::MNRLState(
	shared_ptr<vector<pair<string,string>>> outputSymbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	bool latched,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(outputSymbols),
		attributes
), outputSymbols(outputSymbols), reportId(MNRLReportId()), latched(latched) {}

MNRLState::~MNRLState() {}

Json MNRLState::to_json() {
	Json parent = MNRLNode::to_json();

	// we know that this is an obj
	map<string, Json> mapping = parent.object_items();

	// insert the type
	mapping.insert(map<string, Json>::value_type("type", Json("state")));

	// get the attributes
	map<string, Json> attrs = mapping["attributes"].object_items();

	map<string, Json> symbolSet;
	for(auto &s : *outputSymbols) {
		// add the symbolSet for the given port
		symbolSet.insert(map<string,Json>::value_type(s.first, Json(s.second)));
	}

	// insert the symbolSets
	attrs.insert(map<string, Json>::value_type("symbolSet", Json(symbolSet)));

	// insert reportId
	attrs.insert(map<string, Json>::value_type("reportId", Json(reportId)));

	// insert latched
	attrs.insert(map<string, Json>::value_type("latched", Json(latched)));

	// update the attributes
	mapping["attributes"] = Json(attrs);

	return Json(mapping);
}

MNRLReportId MNRLState::getReportId() { return reportId; }
void MNRLState::setReportId(string id) { reportId = MNRLReportIdString(id); }
void MNRLState::setReportId(int id) { reportId = MNRLReportIdInt(id); }
void MNRLState::setReportId(MNRLReportId id) { reportId = id; }

bool MNRLState::getLatched() { return latched; }
void MNRLState::setLatched(bool l) { latched = l; }
