// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLReportId Object

#ifndef MNRLREPORTID_HPP
#define MNRLREPORTID_HPP

#include "MNRLDefs.hpp"
#include <string>

namespace MNRL {
	class MNRLReportId {
		public:
			MNRLReportId();
			virtual ~MNRLReportId();
			virtual MNRLDefs::ReportIdType get_type();
			virtual std::string toString();
	};

	class MNRLReportIdInt : public MNRLReportId {
		public:
			MNRLReportIdInt(int id);
			virtual ~MNRLReportIdInt();
			int getId();
			virtual MNRLDefs::ReportIdType get_type();
			virtual std::string toString();
		private:
			int id;
	};

	class MNRLReportIdString : public MNRLReportId {
		public:
				MNRLReportIdString(std::string id);
				virtual ~MNRLReportIdString();
				std::string getId();
				virtual MNRLDefs::ReportIdType get_type();
				virtual std::string toString();
			private:
				std::string id;
	};
}

#endif
