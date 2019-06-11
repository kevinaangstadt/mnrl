// Kevin Angstadt
// angstadt {at} umich.edu
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
#include "MNRLReportId.hpp"

namespace MNRL {
	class MNRLBoolean : public MNRLNode {
	public:
		MNRLBoolean (
			MNRLDefs::BooleanMode mode,
			unsigned int portCount,
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			int reportId,
			std::map<std::string,std::string> attributes
		) : MNRLNode (
			id,
			enable,
			report,
			gen_input(portCount),
			gen_output(),
			attributes
		),  mode(mode), reportId(new MNRLReportIdInt(reportId)) {}
		
		MNRLBoolean (
			MNRLDefs::BooleanMode mode,
			unsigned int portCount,
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			std::string reportId,
			std::map<std::string,std::string> attributes
		) : MNRLNode (
			id,
			enable,
			report,
			gen_input(portCount),
			gen_output(),
			attributes
		),  mode(mode), reportId(new MNRLReportIdString(reportId)) {}
		
		MNRLBoolean (
			MNRLDefs::BooleanMode mode,
			unsigned int portCount,
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			std::map<std::string,std::string> attributes
		) : MNRLNode (
			id,
			enable,
			report,
			gen_input(portCount),
			gen_output(),
			attributes
		),  mode(mode), reportId(new MNRLReportId()) {}
		
		virtual ~MNRLBoolean() {
			delete reportId;
			reportId = nullptr;
		}
		
		virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::BOOLEAN; }
		
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
		
		MNRLDefs::BooleanMode getMode() { return mode; }
		void setMode(MNRLDefs::BooleanMode m) { mode = m; }
		
	protected:
		int threshold;
		MNRLDefs::BooleanMode mode;
		MNRLReportId *reportId;
		
	private:
		static port_def gen_input(unsigned int port_count) {
			port_def in;
			for(unsigned int i=0; i<port_count; ++i) {
				in.emplace_back(
					"b" + std::to_string(i),
					1
				);
			}
			return in;
		}
		static port_def gen_output() {
			port_def outs;
			outs.emplace_back(
				MNRLDefs::BOOLEAN_OUTPUT,
				1
			);
			return outs;
		}
	};
}

#endif
