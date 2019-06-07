// Kevin Angstadt
// angstadt {at} umich.edu
//
// MNRLHState Object

#ifndef MNRLHSTATE_HPP
#define MNRLHSTATE_HPP

#include <string>
#include <utility>
#include <vector>
#include <map>

#include "MNRLDefs.hpp"
#include "MNRLNode.hpp"
#include "MNRLReportId.hpp"

namespace MNRL {
	class MNRLHState : public MNRLNode {
	public:
		MNRLHState(
			std::string symbols,
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
				gen_output(),
				attributes
		), symbols(symbols), reportId(new MNRLReportIdInt(reportId)), latched(latched) {}
		
		MNRLHState(
			std::string symbols,
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
				gen_output(),
				attributes
		), symbols(symbols), reportId(new MNRLReportIdString(reportId)), latched(latched) {}
		
		MNRLHState(
			std::string symbols,
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
				gen_output(),
				attributes
		), symbols(symbols), reportId(new MNRLReportId()), latched(latched) {}
		
		virtual ~MNRLHState() {
			delete reportId;
			reportId = nullptr;
		}
		
		virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::HSTATE; }
		
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
		
		const std::string &getSymbolSet() const { return symbols; }
		void setSymbolSet(std::string &set) { symbols = set; }
		
	protected:
		std::string symbols;
		bool latched;
		MNRLReportId *reportId;
		
	private:
		static port_def gen_input() {
			port_def in;
			in.emplace_back(
				MNRLDefs::H_STATE_INPUT,
				1
			);
			return in;
		}
		static port_def gen_output() {
			port_def outs;
			outs.emplace_back(
				MNRLDefs::H_STATE_OUTPUT,
				1
			);
			return outs;
		}
	};
}

#endif
