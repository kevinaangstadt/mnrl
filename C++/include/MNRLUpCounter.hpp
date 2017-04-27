// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLUpCounter Object

#ifndef MNRLUPCOUNTER_HPP
#define MNRLUPCOUNTER_HPP

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
	class MNRLUpCounter : public MNRLNode {
		public:
			MNRLUpCounter(
				int threshold,
				MNRLDefs::CounterMode mode,
				std::string id,
				MNRLDefs::EnableType enable,
				bool report,
				std::shared_ptr<MNRLReportId> reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLUpCounter(
				int threshold,
				MNRLDefs::CounterMode mode,
				std::string id,
				MNRLDefs::EnableType enable,
				bool report,
				int reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLUpCounter(
				int threshold,
				MNRLDefs::CounterMode mode,
				std::string id,
				MNRLDefs::EnableType enable,
				bool report,
				std::string reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLUpCounter(
				int threshold,
				MNRLDefs::CounterMode mode,
				std::string id,
				MNRLDefs::EnableType enable,
				bool report,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			virtual ~MNRLUpCounter();

			virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::UPCOUNTER; }

			std::shared_ptr<MNRLReportId> getReportId();
			void setReportId(std::string id);
			void setReportId(int id);
			void setReportId(std::shared_ptr<MNRLReportId> id);

			MNRLDefs::CounterMode getMode();
			void setMode(MNRLDefs::CounterMode m);

			int getThreshold();
			void setThreshold(int t);

		protected:
			int threshold;
			MNRLDefs::CounterMode mode;
			std::shared_ptr<MNRLReportId> reportId;

		private:
			static port_def gen_input() {
				port_def in;
				in.push_back(
					std::shared_ptr<MNRLPort>( new MNRLPort(
							MNRLDefs::UP_COUNTER_COUNT,
							1
						)
					)
				);
				in.push_back(
					std::shared_ptr<MNRLPort>( new MNRLPort(
							MNRLDefs::UP_COUNTER_RESET,
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
							MNRLDefs::UP_COUNTER_OUTPUT,
							1
						)
					)
				);
				return outs;
			}
	};
}

#endif
