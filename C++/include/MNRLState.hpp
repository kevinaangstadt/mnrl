// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLState Object

#ifndef MNRLSTATE_HPP
#define MNRLSTATE_HPP

#include <string>
#include <utility>
#include <vector>
#include <map>
#include <json11.hpp>

#include "MNRLDefs.hpp"
#include "MNRLNode.hpp"
#include "MNRLPort.hpp"
#include "MNRLReportId.hpp"

namespace MNRL {
	class MNRLDefs;
	class MNRLNode;
	class MNRLPort;
	class MNRLState : public MNRLNode {
		public:
			MNRLState(
				std::shared_ptr<std::vector<std::pair<std::string,std::string>>> outputSymbols,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				bool latched,
				std::shared_ptr<MNRLReportId> reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLState(
				std::shared_ptr<std::vector<std::pair<std::string,std::string>>> outputSymbols,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				bool latched,
				int reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLState(
				std::shared_ptr<std::vector<std::pair<std::string,std::string>>> outputSymbols,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				bool latched,
				std::string reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);
			MNRLState(
				std::shared_ptr<std::vector<std::pair<std::string,std::string>>> outputSymbols,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				bool latched,
				std::shared_ptr<std::map<std::string,std::string>> attributes
			);

			virtual ~MNRLState();

			virtual json11::Json to_json();

			virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::STATE; }

			std::shared_ptr<MNRLReportId> getReportId();
			void setReportId(std::string id);
			void setReportId(int id);
			void setReportId(std::shared_ptr<MNRLReportId> id);

			bool getLatched();
			void setLatched(bool l);

		protected:
			std::shared_ptr<std::vector<std::pair<std::string,std::string>>> outputSymbols;
			bool latched;
			std::shared_ptr<MNRLReportId> reportId;

		private:
			static port_def gen_input() {
				port_def in;
				in.push_back(
					std::shared_ptr<MNRLPort>( new MNRLPort(
							MNRLDefs::STATE_INPUT,
							1
						)
					)
				);
				return in;
			}
			static port_def gen_output(std::shared_ptr<std::vector<std::pair<std::string,std::string>>> &outputSymbols) {
				port_def outs;
				for(auto &o_s : *outputSymbols) {
					outs.push_back(
						std::shared_ptr<MNRLPort>(new MNRLPort(o_s.first, 1))
					);
				}
				return outs;
			}
	};
}

#endif
