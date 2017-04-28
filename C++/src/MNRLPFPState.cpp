/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLPFPState.cpp
 */

#include "MNRLPFPState.hpp"

using namespace std;
using namespace MNRL;

MNRLPFPState::MNRLPFPState(
        int feature,
        double threshold,
        bool greaterThan,
        MNRLDefs::EnableType enable,
        string id,
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
), feature(feature),
   threshold(threshold),
   greaterThan(greaterThan),
   reportId(reportId) {}

MNRLPFPState::MNRLPFPState(
	int feature,
    double threshold,
    bool greaterThan,
    MNRLDefs::EnableType enable,
    string id,
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
), feature(feature),
   threshold(threshold),
   greaterThan(greaterThan),
   reportId(shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(reportId))) {}

MNRLPFPState::MNRLPFPState(
    int feature,
    double threshold,
    bool greaterThan,
    MNRLDefs::EnableType enable,
    string id,
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
), feature(feature),
   threshold(threshold),
   greaterThan(greaterThan),
   reportId(shared_ptr<MNRLReportIdString>(new MNRLReportIdString(reportId))) {}


MNRLPFPState::MNRLPFPState(
	int feature,
    double threshold,
    bool greaterThan,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    shared_ptr<map<string,string>> attributes
) : MNRLNode (
		id,
		enable,
		report,
		gen_input(),
		gen_output(),
		attributes
), feature(feature),
   threshold(threshold),
   greaterThan(greaterThan),
   reportId(shared_ptr<MNRLReportId>(new MNRLReportId())) {}

MNRLPFPState::~MNRLPFPState() {}

shared_ptr<MNRLReportId> MNRLPFPState::getReportId() { return reportId; }
void MNRLPFPState::setReportId(string id) { reportId = shared_ptr<MNRLReportIdString>(new MNRLReportIdString(id)); }
void MNRLPFPState::setReportId(int id) { reportId = shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(id)); }
void MNRLPFPState::setReportId(shared_ptr<MNRLReportId> id) { reportId = id; }

int MNRLPFPState::getFeature() { return feature; }
void MNRLPFPState::setFeature(int f) { feature = f; }

double MNRLPFPState::getThreshold() { return threshold; }
void MNRLPFPState::setThreshold(double t) { threshold = t; }

bool MNRLPFPState::getGreaterThan() { return greaterThan; }
void MNRLPFPState::setGreaterThan(bool g) { greaterThan = g; }
