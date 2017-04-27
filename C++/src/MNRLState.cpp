/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLState.cpp
 */

#include "MNRLState.hpp"

using namespace std;
using namespace MNRL;

MNRLState::MNRLState(
	shared_ptr<vector<pair<string,string>>> outputSymbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	bool latched,
	shared_ptr<MNRLReportId> reportId,
	shared_ptr<map<string,string>> attributes
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
	shared_ptr<map<string,string>> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(outputSymbols),
		attributes
), outputSymbols(outputSymbols), reportId(shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(reportId))), latched(latched) {}

MNRLState::MNRLState(
	shared_ptr<vector<pair<string,string>>> outputSymbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	bool latched,
	string reportId,
	shared_ptr<map<string,string>> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(outputSymbols),
		attributes
), outputSymbols(outputSymbols), reportId(shared_ptr<MNRLReportIdString>(new MNRLReportIdString(reportId))), latched(latched) {}

MNRLState::MNRLState(
	shared_ptr<vector<pair<string,string>>> outputSymbols,
	MNRLDefs::EnableType enable,
	string id,
	bool report,
	bool latched,
	shared_ptr<map<string,string>> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(outputSymbols),
		attributes
), outputSymbols(outputSymbols), reportId(shared_ptr<MNRLReportId>(new MNRLReportId())), latched(latched) {}

MNRLState::~MNRLState() {}

shared_ptr<MNRLReportId> MNRLState::getReportId() { return reportId; }
void MNRLState::setReportId(string id) { reportId = shared_ptr<MNRLReportIdString>(new MNRLReportIdString(id)); }
void MNRLState::setReportId(int id) { reportId = shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(id)); }
void MNRLState::setReportId(shared_ptr<MNRLReportId> id) { reportId = id; }

bool MNRLState::getLatched() { return latched; }
void MNRLState::setLatched(bool l) { latched = l; }

shared_ptr<vector<pair<string,string>>> MNRLState::getOutputSymbols() { return outputSymbols; }
