/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLReportId.cpp
 */

#include "MNRLReportId.hpp"

using namespace std;
using namespace MNRL;

MNRLReportId::MNRLReportId() {}
MNRLReportId::~MNRLReportId() {}
MNRLDefs::ReportIdType MNRLReportId::get_type() { return MNRLDefs::ReportIdType::NONE; }
string MNRLReportId::toString() { return ""; }

MNRLReportIdInt::MNRLReportIdInt(int id) : MNRLReportId(), id(id) {}
MNRLReportIdInt::~MNRLReportIdInt() {}
int MNRLReportIdInt::getId() { return id; }
MNRLDefs::ReportIdType MNRLReportIdInt::get_type() { return MNRLDefs::ReportIdType::INT; }
string MNRLReportIdInt::toString() { return std::to_string(id); }


MNRLReportIdString::MNRLReportIdString(string id) : MNRLReportId(), id(id) {}
MNRLReportIdString::~MNRLReportIdString() {}
string MNRLReportIdString::getId() { return id; }
MNRLDefs::ReportIdType MNRLReportIdString::get_type() { return MNRLDefs::ReportIdType::STRING; }
string MNRLReportIdString::toString() { return id; }
