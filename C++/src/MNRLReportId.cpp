/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLReportId.cpp
 */

#include "MNRLReportId.hpp"

using namespace std;
using namespace MNRL;
using namespace json11;

MNRLReportId::MNRLReportId() {}
MNRLReportId::~MNRLReportId() {}
Json MNRLReportId::to_json() const {
	return Json();
}

MNRLReportIdInt::MNRLReportIdInt(int id) : MNRLReportId(), id(id) {}
MNRLReportIdInt::~MNRLReportIdInt() {}
int MNRLReportIdInt::getId() { return id; }
Json MNRLReportIdInt::to_json() const {
	return Json(id);
}


MNRLReportIdString::MNRLReportIdString(string id) : MNRLReportId(), id(id) {}
MNRLReportIdString::~MNRLReportIdString() {}
string MNRLReportIdString::getId() { return id; }
Json MNRLReportIdString::to_json() const {
	return Json(id);
}
