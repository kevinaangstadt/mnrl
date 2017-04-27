// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLBoolean Object

#ifndef MNRLBOOLEAN_HPP
#define MNRLBOOLEAN_HPP

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
	class MNRLBoolean : public MNRLNode {
		public:
			MNRLBoolean (
				MNRLDefs::BooleanMode mode,
				int portCount,
				std::string id,
				MNRLDefs::EnableType enable,
				bool report,
				std::shared_ptr<MNRLReportId> reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLBoolean (
				MNRLDefs::BooleanMode mode,
				int portCount,
				std::string id,
				MNRLDefs::EnableType enable,
				bool report,
				int reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLBoolean (
				MNRLDefs::BooleanMode mode,
				int portCount,
				std::string id,
				MNRLDefs::EnableType enable,
				bool report,
				std::string reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLBoolean (
				MNRLDefs::BooleanMode mode,
				int portCount,
				std::string id,
				MNRLDefs::EnableType enable,
				bool report,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			virtual ~MNRLBoolean();

			virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::BOOLEAN; }

			std::shared_ptr<MNRLReportId> getReportId();
			void setReportId(std::string id);
			void setReportId(int id);
			void setReportId(std::shared_ptr<MNRLReportId> id);

			MNRLDefs::BooleanMode getMode();
			void setMode(MNRLDefs::BooleanMode m);

		protected:
			int threshold;
			MNRLDefs::BooleanMode mode;
			std::shared_ptr<MNRLReportId> reportId;

		private:
			static port_def gen_input(int port_count) {
				port_def in;
				for(int i=0; i<port_count; ++i) {
					in.push_back(
						std::shared_ptr<MNRLPort>( new MNRLPort(
								"b" + std::to_string(i),
								1
							)
						)
					);
				}
				return in;
			}
			static port_def gen_output() {
				port_def outs;
				outs.push_back(
					std::shared_ptr<MNRLPort>( new MNRLPort(
							MNRLDefs::BOOLEAN_OUTPUT,
							1
						)
					)
				);
				return outs;
			}
	};
}

#endif
