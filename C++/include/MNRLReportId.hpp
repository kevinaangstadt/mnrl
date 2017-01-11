// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLReportId Object

#ifndef MNRLREPORTID_HPP
#define MNRLREPORTID_HPP

#include <string>
#include <json11.hpp>

namespace MNRL {
	class MNRLReportId {
		public:
			MNRLReportId();
			virtual ~MNRLReportId();
			virtual json11::Json to_json() const;
	};

	class MNRLReportIdInt : public MNRLReportId {
		public:
			MNRLReportIdInt(int id);
			virtual ~MNRLReportIdInt();
			virtual json11::Json to_json() const;
			int getId();
		private:
			int id;
	};

	class MNRLReportIdString : public MNRLReportId {
		public:
				MNRLReportIdString(std::string id);
				virtual ~MNRLReportIdString();
				virtual json11::Json to_json() const;
				std::string getId();
			private:
				std::string id;
	};
}

#endif
