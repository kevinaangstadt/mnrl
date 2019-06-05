// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLNetwork Object

#ifndef MNRLNETWORK_HPP
#define MNRLNETWORK_HPP

#include <string>
#include <utility>
#include <vector>

#include "MapIterator.hpp"

#include "MNRLState.hpp"
#include "MNRLHState.hpp"
#include "MNRLUpCounter.hpp"
#include "MNRLBoolean.hpp"
#include "MNRLDefs.hpp"
#include "MNRLError.hpp"


namespace MNRL {
	using std::ofstream;
	class MNRLNetwork {
	public:
		MNRLNetwork(std::string id) : id(id), nodes_added(0) {}
		virtual ~MNRLNetwork() {}
		virtual void exportToFile(std::string filename, bool pretty=false);
		
		MapIterator<std::map<std::string,MNRLNode*>> getNodeIterator() {
			return MapIterator<std::map<std::string, MNRLNode*>>(nodes);
		}
		std::map<std::string,MNRLNode*> getNodes() { return nodes; }
		
		MNRLNode &getNodeById(const std::string &id) {
			auto it = nodes.find(id);
			if (it != nodes.end()) {
				return *(it->second);
			}
			else {
				throw MNRLError::UnknownNode(id);
			}
		}
		
		MNRLNode &addNode(MNRLNode &theNode) {
			std::string id = getUniqueNodeId(theNode.getId());
			theNode.setId(id);
			nodes[id] = new MNRLNode(theNode);
			return *(nodes[id]);
		}
		
		MNRLNode &addNode(
			std::string id, 
			MNRLDefs::EnableType enable,
			bool report,
			port_def ins,
			port_def outs,
			std::map<std::string,std::string> attrs
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLNode(id, enable, report, ins, outs, attrs);
			return *(nodes[new_id]);
		}
		
		MNRLState &addState(
			std::vector<std::pair<std::string,std::string>> outputSymbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			int reportId,
			bool latched,
			std::map<std::string,std::string> attributes
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLState(outputSymbols, enable, new_id, report, reportId, latched, attributes);
			return *(dynamic_cast<MNRLState*>(nodes[new_id]));
		}
		
		MNRLState &addState(
			std::vector<std::pair<std::string,std::string>> outputSymbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			std::string reportId,
			bool latched,
			std::map<std::string,std::string> attributes
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLState(outputSymbols, enable, new_id, report, reportId, latched, attributes);
			return *(dynamic_cast<MNRLState*>(nodes[new_id]));
		}
		
		MNRLState &addState(
			std::vector<std::pair<std::string,std::string>> outputSymbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			int reportId,
			bool latched
		) {
			return addState(
				outputSymbols,
				enable,
				id,
				report,
				reportId,
				latched,
				std::map<std::string,std::string>()
			);
		}
		
		MNRLHState &addHState(
			std::string symbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			int reportId,
			bool latched,
			std::map<std::string,std::string> attributes
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLHState(symbols, enable, new_id, report, reportId, latched,  attributes);
			return *(dynamic_cast<MNRLHState*>(nodes[new_id]));
		}
		
		MNRLHState &addHState(
			std::string symbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			std::string reportId,
			bool latched,
			std::map<std::string,std::string> attributes
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLHState(symbols, enable, new_id, report, reportId, latched, attributes);
			return *(dynamic_cast<MNRLHState*>(nodes[new_id]));
		}
		
		MNRLHState &addHState(
			std::string symbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			bool latched,
			std::map<std::string,std::string> attributes
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLHState(symbols, enable, new_id, report, latched, attributes);
			return *(dynamic_cast<MNRLHState*>(nodes[new_id]));
		}
		
		MNRLHState &addHState(
			std::string symbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			int reportId,
			bool latched
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLHState(symbols, enable, new_id, report, reportId, latched, std::map<std::string,std::string>());
			return *(dynamic_cast<MNRLHState*>(nodes[new_id]));
		}
		
		MNRLHState &addHState(
			std::string symbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			std::string reportId,
			bool latched
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLHState(symbols, enable, new_id, report, reportId, latched, std::map<std::string,std::string>());
			return *(dynamic_cast<MNRLHState*>(nodes[new_id]));
		}
		
		MNRLHState &addHState(
			std::string symbols,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			bool latched
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLHState(symbols, enable, new_id, report, latched, std::map<std::string,std::string>());
			return *(dynamic_cast<MNRLHState*>(nodes[new_id]));
		}
		
		MNRLUpCounter &addUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			bool report,
			int reportId,
			std::map<std::string,std::string> attributes
		) {
			if(threshold < 0) {
				throw MNRLError::UpCounterThresholdError(threshold);
			}
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLUpCounter(threshold, mode, new_id, MNRLDefs::EnableType::ENABLE_ON_ACTIVATE_IN, report, reportId, attributes);
			return *(dynamic_cast<MNRLUpCounter*>(nodes[new_id]));
		}
		
		MNRLUpCounter &addUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			bool report,
			int reportId
		) {
			if(threshold < 0) {
				throw MNRLError::UpCounterThresholdError(threshold);
			}
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLUpCounter(threshold, mode, new_id, MNRLDefs::EnableType::ENABLE_ON_ACTIVATE_IN, report, reportId, std::map<std::string,std::string>());
			return *(dynamic_cast<MNRLUpCounter*>(nodes[new_id]));
		}
		
		MNRLUpCounter &addUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			int reportId,
			std::map<std::string,std::string> attributes
		) {
			if(threshold < 0) {
				throw MNRLError::UpCounterThresholdError(threshold);
			}
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLUpCounter(threshold, mode, new_id, enable, report, reportId, attributes);
			return *(dynamic_cast<MNRLUpCounter*>(nodes[new_id]));
		}
		
		MNRLUpCounter &addUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			int reportId
		) {
			if(threshold < 0) {
				throw MNRLError::UpCounterThresholdError(threshold);
			}
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLUpCounter(threshold, mode, new_id, enable, report, reportId, std::map<std::string,std::string>());
			return *(dynamic_cast<MNRLUpCounter*>(nodes[new_id]));
		}
		
		MNRLUpCounter &addUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			bool report,
			std::string reportId,
			std::map<std::string,std::string> attributes
		) {
			if(threshold < 0) {
				throw MNRLError::UpCounterThresholdError(threshold);
			}
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLUpCounter(threshold, mode, new_id, MNRLDefs::EnableType::ENABLE_ON_ACTIVATE_IN, report, reportId, attributes);
			return *(dynamic_cast<MNRLUpCounter*>(nodes[new_id]));
		}
		
		MNRLUpCounter &addUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			bool report,
			std::string reportId
		) {
			if(threshold < 0) {
				throw MNRLError::UpCounterThresholdError(threshold);
			}
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLUpCounter(threshold, mode, new_id, MNRLDefs::EnableType::ENABLE_ON_ACTIVATE_IN, report, reportId, std::map<std::string,std::string>());
			return *(dynamic_cast<MNRLUpCounter*>(nodes[new_id]));
		}
		
		MNRLUpCounter &addUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			std::string reportId,
			std::map<std::string,std::string> attributes
		) {
			if(threshold < 0) {
				throw MNRLError::UpCounterThresholdError(threshold);
			}
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLUpCounter(threshold, mode, new_id, enable, report, reportId, attributes);
			return *(dynamic_cast<MNRLUpCounter*>(nodes[new_id]));
		}
		
		MNRLUpCounter &addUpCounter(
			int threshold,
			MNRLDefs::CounterMode mode,
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			std::string reportId
		) {
			if(threshold < 0) {
				throw MNRLError::UpCounterThresholdError(threshold);
			}
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLUpCounter(threshold, mode, new_id, enable, report, reportId, std::map<std::string,std::string>());
			return *(dynamic_cast<MNRLUpCounter*>(nodes[new_id]));
		}
		
		MNRLBoolean &addBoolean(
			MNRLDefs::BooleanMode booleanType,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			int reportId,
			std::map<std::string,std::string> attributes
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLBoolean(booleanType, MNRLDefs::BooleanToPort(booleanType), new_id, enable, report, reportId, attributes);
			return *(dynamic_cast<MNRLBoolean*>(nodes[new_id]));
		}
		
		MNRLBoolean &addBoolean(
			MNRLDefs::BooleanMode booleanType,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			int reportId
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLBoolean(booleanType, MNRLDefs::BooleanToPort(booleanType), new_id, enable, report, reportId, std::map<std::string,std::string>());
			return *(dynamic_cast<MNRLBoolean*>(nodes[new_id]));
		}
		
		MNRLBoolean &addBoolean(
			MNRLDefs::BooleanMode booleanType,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			std::string reportId,
			std::map<std::string,std::string> attributes
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLBoolean(booleanType, MNRLDefs::BooleanToPort(booleanType), new_id, enable, report, reportId, attributes);
			return *(dynamic_cast<MNRLBoolean*>(nodes[new_id]));
		}
		
		MNRLBoolean &addBoolean(
			MNRLDefs::BooleanMode booleanType,
			MNRLDefs::EnableType enable,
			std::string id,
			bool report,
			std::string reportId
		) {
			std::string new_id = getUniqueNodeId(id);
			nodes[new_id] = new MNRLBoolean(booleanType, MNRLDefs::BooleanToPort(booleanType), new_id, enable, report, reportId, std::map<std::string,std::string>());
			return *(dynamic_cast<MNRLBoolean*>(nodes[new_id]));
		}
		
		void addConnection(std::string src, std::string src_port, std::string dest, std::string dest_port){
			std::string s_id = src;
			MNRLNode &s_node = getNodeById(src);
			MNRLPort &s_port = s_node.getOutputPort(src_port);
			int s_output_width = s_port.getWidth();

			std::string d_id = dest;
			MNRLNode &d_node = getNodeById(dest);
			MNRLPort &d_port = d_node.getInputPort(dest_port);
			int d_input_width = d_port.getWidth();

			if(s_output_width != d_input_width)
				throw MNRLError::PortWidthMismatch(s_output_width, d_input_width);

			s_port.addConnection(dest,d_port.getId());
			d_port.addConnection(src,s_port.getId());

		}
		
		void removeConnection(std::string src, std::string src_port, std::string dest, std::string dest_port) {
			std::string s_id = src;
			MNRLNode &s_node = getNodeById(s_id);
			MNRLPort &s_port = s_node.getOutputPort(src_port);

			std::string d_id = dest;
			MNRLNode &d_node = getNodeById(d_id);
			MNRLPort &d_port = d_node.getInputPort(src_port);

			s_port.deleteConnection(dest,dest_port);
			d_port.deleteConnection(src,src_port);
		}

		
		const std::string &getId() const { return id; }
		
	protected:
		std::string id;
		std::map <std::string,MNRLNode*> nodes;
		unsigned long nodes_added;
		
		std::string getUniqueNodeId(std::string id) {
			if(id.length() == 0) {
				std::string ret = "_" + std::to_string(nodes_added);
				nodes_added += 1;
				return ret;
			}

			auto it = nodes.find(id);
			if(it != nodes.end())
				throw MNRLError::DuplicateIdError(id);

			return id;
		}
	};
}
#endif
