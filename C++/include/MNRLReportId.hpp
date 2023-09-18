// Kevin Angstadt
// kangstadt {at} stlawu.edu
//
// MNRLReportId Object

#ifndef MNRLREPORTID_HPP
#define MNRLREPORTID_HPP

#include "MNRLDefs.hpp"
#include <string>

namespace MNRL {
	class MNRLReportId {
		public:
			MNRLReportId() {}
			virtual ~MNRLReportId() {}
			virtual MNRLDefs::ReportIdType get_type() { return MNRLDefs::ReportIdType::NONE; }
			virtual std::string toString() { return ""; }
			virtual MNRLReportId* copy() { return new MNRLReportId(); }
	};

	class MNRLReportIdInt : public MNRLReportId {
		public:
			MNRLReportIdInt(int id) : MNRLReportId(), id(id) {}
			virtual ~MNRLReportIdInt() {}
			int getId() { return id; }
			virtual MNRLDefs::ReportIdType get_type() { return MNRLDefs::ReportIdType::INT; }
			virtual std::string toString() { return std::to_string(id); }
			virtual MNRLReportId* copy() { return new MNRLReportIdInt(getId()); }
		private:
			int id;
	};

	class MNRLReportIdString : public MNRLReportId {
		public:
				MNRLReportIdString(std::string id) : MNRLReportId(), id(id) {}
				virtual ~MNRLReportIdString() {}
				std::string &getId() { return id; }
				virtual MNRLDefs::ReportIdType get_type() { return MNRLDefs::ReportIdType::STRING; }
				virtual std::string toString() { return id; }
				virtual MNRLReportId* copy() { return new MNRLReportIdString(getId()); }
			private:
				std::string id;
	};
}

#endif
