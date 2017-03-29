/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLUpCounter.cpp
 */

#include "MNRLUpCounter.hpp"

using namespace std;
using namespace MNRL;
using namespace json11;

MNRLUpCounter::MNRLUpCounter(
	int threshold,
	MNRLDefs::CounterMode mode,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	shared_ptr<MNRLReportId> reportId,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(),
		attributes
), threshold(threshold), mode(mode), reportId(reportId) {}

MNRLUpCounter::MNRLUpCounter(
	int threshold,
	MNRLDefs::CounterMode mode,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	int reportId,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(),
		attributes
), threshold(threshold), mode(mode), reportId(shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(reportId))) {}

MNRLUpCounter::MNRLUpCounter(
	int threshold,
	MNRLDefs::CounterMode mode,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	string reportId,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(),
		attributes
), threshold(threshold), mode(mode), reportId(shared_ptr<MNRLReportIdString>(new MNRLReportIdString(reportId))) {}

MNRLUpCounter::MNRLUpCounter(
	int threshold,
	MNRLDefs::CounterMode mode,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	shared_ptr<Json::object> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(),
		attributes
), threshold(threshold), mode(mode), reportId(shared_ptr<MNRLReportId>(new MNRLReportId())) {}

MNRLUpCounter::~MNRLUpCounter() {}

Json MNRLUpCounter::to_json() {
	Json parent = MNRLNode::to_json();

	// we know that this is an obj
	map<string, Json> mapping = parent.object_items();

	// insert the type
	mapping.insert(map<string, Json>::value_type("type", Json("upCounter")));

	// get the attributes
	map<string, Json> attrs = mapping["attributes"].object_items();

	// insert the symbolSets
	attrs.insert(map<string, Json>::value_type("threshold", Json(threshold)));

	// insert reportId
	attrs.insert(map<string, Json>::value_type("reportId", Json(*reportId)));

	// insert latched
	attrs.insert(map<string, Json>::value_type("mode", Json(MNRLDefs::toMNRLCounterMode(mode))));

	// update the attributes
	mapping["attributes"] = Json(attrs);

	return Json(mapping);
}

shared_ptr<MNRLReportId> MNRLUpCounter::getReportId() { return reportId; }
void MNRLUpCounter::setReportId(string id) { reportId = shared_ptr<MNRLReportIdString>( new MNRLReportIdString(id)); }
void MNRLUpCounter::setReportId(int id) { reportId = shared_ptr<MNRLReportIdInt>( new MNRLReportIdInt(id)); }
void MNRLUpCounter::setReportId(shared_ptr<MNRLReportId> id) { reportId = id; }
