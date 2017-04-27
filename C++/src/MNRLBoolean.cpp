/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLUpCounter.cpp
 */

#include "MNRLBoolean.hpp"

using namespace std;
using namespace MNRL;

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

shared_ptr<MNRLReportId> MNRLBoolean::getReportId() { return reportId; }
void MNRLBoolean::setReportId(string id) { reportId = shared_ptr<MNRLReportIdString>(new MNRLReportIdString(id)); }
void MNRLBoolean::setReportId(int id) { reportId = shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(id)); }
void MNRLBoolean::setReportId(shared_ptr<MNRLReportId> id) { reportId = id; }

MNRLDefs::BooleanMode MNRLBoolean::getMode() { return mode; }
void MNRLBoolean::setMode(MNRLDefs::BooleanMode m) { mode = m; }
