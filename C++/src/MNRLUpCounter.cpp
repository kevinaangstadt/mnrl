/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLUpCounter.cpp
 */

#include "MNRLUpCounter.hpp"

using namespace std;
using namespace MNRL;

MNRLUpCounter::MNRLUpCounter(
	int threshold,
	MNRLDefs::CounterMode mode,
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	shared_ptr<MNRLReportId> reportId,
	shared_ptr<map<string,string>> attributes
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
	shared_ptr<map<string,string>> attributes
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
	shared_ptr<map<string,string>> attributes
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
	shared_ptr<map<string,string>> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(),
		attributes
), threshold(threshold), mode(mode), reportId(shared_ptr<MNRLReportId>(new MNRLReportId())) {}

MNRLUpCounter::~MNRLUpCounter() {}

shared_ptr<MNRLReportId> MNRLUpCounter::getReportId() { return reportId; }
void MNRLUpCounter::setReportId(string id) { reportId = shared_ptr<MNRLReportIdString>( new MNRLReportIdString(id)); }
void MNRLUpCounter::setReportId(int id) { reportId = shared_ptr<MNRLReportIdInt>( new MNRLReportIdInt(id)); }
void MNRLUpCounter::setReportId(shared_ptr<MNRLReportId> id) { reportId = id; }

MNRLDefs::CounterMode MNRLUpCounter::getMode() { return mode; }
void MNRLUpCounter::setMode(MNRLDefs::CounterMode m) { mode = m; }

int MNRLUpCounter::getThreshold() { return threshold; }
void MNRLUpCounter::setThreshold(int t) { threshold = t; };
