// Kevin Angstadt
// kangstadt {at} stlawu.edu
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
#include "MNRLReportId.hpp"

namespace MNRL {
	class MNRLUpCounter : public MNRLNode {
	public:
		MNRLUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			int reportId,
			std::map<std::string,std::string> attributes
		) : MNRLNode (
			id,
			enable,
			report,
			gen_input(),
			gen_output(),
			attributes
		), threshold(threshold), mode(mode), reportId(new MNRLReportIdInt(reportId)) {}
		
		MNRLUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			std::string reportId,
			std::map<std::string,std::string> attributes
		) : MNRLNode (
			id,
			enable,
			report,
			gen_input(),
			gen_output(),
			attributes
		), threshold(threshold), mode(mode), reportId(new MNRLReportIdString(reportId)) {}
		
		MNRLUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			std::map<std::string,std::string> attributes
		) : MNRLNode (
			id,
			enable,
			report,
			gen_input(),
			gen_output(),
			attributes
		), threshold(threshold), mode(mode), reportId(new MNRLReportId()) {}

		MNRLUpCounter(MNRLUpCounter& other) :
			MNRLNode ( other ),
			threshold(other.threshold),
			mode(other.mode),
			reportId(other.reportId->copy()) {}
		
		virtual ~MNRLUpCounter() {
			delete reportId;
			reportId = nullptr;
		}
		
		virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::UPCOUNTER; }
		
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
		
		MNRLDefs::CounterMode getMode() { return mode; }
		void setMode(MNRLDefs::CounterMode m) { mode = m; }
		
		int getThreshold() { return threshold; }
		void setThreshold(int t) { threshold = t; }
		
	protected:
		int threshold;
		MNRLDefs::CounterMode mode;
		MNRLReportId *reportId;
		
	private:
		static port_def gen_input() {
			port_def in;
			in.emplace_back(
				MNRLDefs::UP_COUNTER_COUNT,
				1
			);
			in.emplace_back(
				MNRLDefs::UP_COUNTER_RESET,
				1
			);
			return in;
		}
		static port_def gen_output() {
			port_def outs;
			outs.emplace_back(
				MNRLDefs::UP_COUNTER_OUTPUT,
				1
			);
			return outs;
		}
	};
}

#endif
