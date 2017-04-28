// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLPFPState Object

#ifndef MNRLPFPSTATE_HPP
#define MNRLPFPSTATE_HPP

#include <string>
#include <utility>
#include <vector>
#include <map>

#include "MNRLDefs.hpp"
#include "MNRLNode.hpp"
#include "MNRLPort.hpp"
#include "MNRLReportId.hpp"

namespace MNRL {
	class MNRLDefs;
	class MNRLNode;
	class MNRLPort;
	class MNRLPFPState : public MNRLNode {
		public:
			MNRLPFPState(
				int feature,
                double threshold,
                bool greaterThan,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				std::shared_ptr<MNRLReportId> reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLPFPState(
                int feature,
                double threshold,
                bool greaterThan,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				int reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLPFPState(
                int feature,
                double threshold,
                bool greaterThan,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				std::string reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLPFPState(
                int feature,
                double threshold,
                bool greaterThan,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			virtual ~MNRLPFPState();

			virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::PFPSTATE; }

			std::shared_ptr<MNRLReportId> getReportId();
			void setReportId(std::string id);
			void setReportId(int id);
			void setReportId(std::shared_ptr<MNRLReportId> id);

			int getFeature();
			void setFeature(int f);
        
            double getThreshold();
            void setThreshold(double t);
        
            bool getGreaterThan();
            void setGreaterThan(bool g);

		protected:
			int feature;
            double threshold;
			bool greaterThan;
			std::shared_ptr<MNRLReportId> reportId;

		private:
			static port_def gen_input() {
				port_def in;
				in.push_back(
					std::shared_ptr<MNRLPort>( new MNRLPort(
							MNRLDefs::PFP_STATE_INPUT,
							1
						)
					)
				);
				return in;
			}
			static port_def gen_output() {
				port_def outs;
				outs.push_back(
					std::shared_ptr<MNRLPort>( new MNRLPort(
							MNRLDefs::PFP_STATE_OUTPUT,
							1
						)
					)
				);
				return outs;
			}
	};
}

#endif
