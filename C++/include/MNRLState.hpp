// Kevin Angstadt
// kangstadt {at} stlawu.edu
//
// MNRLState Object

#ifndef MNRLSTATE_HPP
#define MNRLSTATE_HPP

#include <string>
#include <utility>
#include <vector>
#include <map>

#include "MNRLDefs.hpp"
#include "MNRLNode.hpp"
#include "MNRLReportId.hpp"

namespace MNRL {
	class MNRLState : public MNRLNode {
	public:		
		MNRLState(
			std::vector<std::pair<std::string,std::string>> outputSymbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			int reportId,
			bool latched,
			std::map<std::string,std::string> attributes
		) : MNRLNode (
			id,
			enable,
			report,
			gen_input(),
			gen_output(outputSymbols),
			attributes
		), outputSymbols(outputSymbols), latched(latched), reportId(new MNRLReportIdInt(reportId)) {}
		
		MNRLState(
			std::vector<std::pair<std::string,std::string>> outputSymbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			std::string reportId,
			bool latched,
			std::map<std::string,std::string> attributes
		) : MNRLNode (
			id,
			enable,
			report,
			gen_input(),
			gen_output(outputSymbols),
			attributes
		), outputSymbols(outputSymbols), latched(latched), reportId(new MNRLReportIdString(reportId)) {}
		
		MNRLState(
			std::vector<std::pair<std::string,std::string>> outputSymbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			bool latched,
			std::map<std::string,std::string> attributes
		) : MNRLNode (
			id,
			enable,
			report,
			gen_input(),
			gen_output(outputSymbols),
			attributes
		), outputSymbols(outputSymbols), latched(latched), reportId(new MNRLReportId()) {}

		MNRLState(MNRLState& other) :
			MNRLNode ( other ),
			outputSymbols(other.outputSymbols),
			latched(other.latched),
			reportId(other.reportId->copy()) {}
		
		virtual ~MNRLState() {
			delete reportId;
			reportId = nullptr;
		}
		
		virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::STATE; }
		
		MNRLReportId *getReportId() { return reportId; }
		
		void setReportId(std::string &id) { 
			delete reportId;
			reportId = nullptr;
			reportId = new MNRLReportIdString(id); 
		}
		
		void setReportId(int id) { 
			delete reportId;
			reportId = nullptr;
			reportId = new MNRLReportIdInt(id); 
		}
		
		bool getLatched() { return latched; }
		void setLatched(bool l) { latched = l; }
		
		const std::vector<std::pair<std::string,std::string>> getOutputSymbols() const { return outputSymbols; }
		
	protected:
		std::vector<std::pair<std::string,std::string>> outputSymbols;
		bool latched;
		MNRLReportId *reportId;
		
	private:
		static port_def gen_input() {
			port_def in;
			in.emplace_back(
				MNRLDefs::STATE_INPUT,
				1
			);
			return in;
		}
		static port_def gen_output(std::vector<std::pair<std::string,std::string>> &outputSymbols) {
			port_def outs;
			for(auto &o_s : outputSymbols) {
				outs.emplace_back(
					o_s.first, 1
				);
			}
			return outs;
		}
	};
}

#endif
