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
			virtual json11::Json to_json() const;
			virtual MNRLDefs::ReportIdType get_type();
			virtual std::string toString();
	};

	class MNRLReportIdInt : public MNRLReportId {
		public:
			MNRLReportIdInt(int id);
			virtual ~MNRLReportIdInt();
			virtual json11::Json to_json() const;
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
				virtual json11::Json to_json() const;
				std::string getId();
				virtual MNRLDefs::ReportIdType get_type();
				virtual std::string toString();
			private:
				std::string id;
	};
}

#endif
