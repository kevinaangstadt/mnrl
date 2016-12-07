// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLHState Object

#ifndef MNRLHSTATE_HPP
#define MNRLHSTATE_HPP

#include <string>
#include <utility>
#include <vector>
#include <map>
#include <json11.hpp>

#include "MNRLDefs.hpp"
#include "MNRLNode.hpp"
#include "MNRLPort.hpp"

namespace MNRL {
	class MNRLDefs;
	class MNRLNode;
	class MNRLPort;
	class MNRLHState : public MNRLNode {
		public:
			MNRLHState(
				std::string symbols,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				bool latched,
				int reportId,
				std::shared_ptr<json11::Json::object> attributes
			);
			virtual ~MNRLHState();

			virtual json11::Json to_json();

		protected:
			std::string symbols;
			bool latched;
			int reportId;

		private:
			static port_def gen_input() {
				port_def in;
				in.push_back(
					std::shared_ptr<MNRLPort>( new MNRLPort(
							MNRLDefs::H_STATE_INPUT,
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
							MNRLDefs::H_STATE_OUTPUT,
							1
						)
					)
				);
				return outs;
			}
	};
}

#endif
