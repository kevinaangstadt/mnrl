/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLHState.cpp
 */

#include "MNRLHState.hpp"

using namespace std;
using namespace MNRL;

MNRLHState::MNRLHState(
	string symbols,
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
		gen_output(),
		attributes
), symbols(symbols), reportId(reportId), latched(latched) {}

MNRLHState::MNRLHState(
	string symbols,
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
		gen_output(),
		attributes
), symbols(symbols), reportId(shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(reportId))), latched(latched) {}

MNRLHState::MNRLHState(
	string symbols,
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
		gen_output(),
		attributes
), symbols(symbols), reportId(shared_ptr<MNRLReportIdString>(new MNRLReportIdString(reportId))), latched(latched) {}


MNRLHState::MNRLHState(
	string symbols,
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
		gen_output(),
		attributes
), symbols(symbols), reportId(shared_ptr<MNRLReportId>(new MNRLReportId())), latched(latched) {}

MNRLHState::~MNRLHState() {}

shared_ptr<MNRLReportId> MNRLHState::getReportId() { return reportId; }
void MNRLHState::setReportId(string id) { reportId = shared_ptr<MNRLReportIdString>(new MNRLReportIdString(id)); }
void MNRLHState::setReportId(int id) { reportId = shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(id)); }
void MNRLHState::setReportId(shared_ptr<MNRLReportId> id) { reportId = id; }

bool MNRLHState::getLatched() { return latched; }
void MNRLHState::setLatched(bool l) { latched = l; }

string MNRLHState::getSymbolSet() { return symbols; }
void MNRLHState::setSymbolSet(string set) { symbols = set; }
