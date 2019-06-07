// Kevin Angstadt
// angstadt {at} umich.edu
//
// Functions for Creating JSON from various MNRL objects

#include <rapidjson/document.h>
#include <rapidjson/allocators.h>
#include <vector>

#include "MNRLNetwork.hpp"
#include "MNRLNode.hpp"
#include "MNRLState.hpp"
#include "MNRLHState.hpp"
#include "MNRLBoolean.hpp"
#include "MNRLUpCounter.hpp"

namespace MNRL {
	
	using namespace rapidjson;
	
	class JSONWriter {
	public:
		static Document toJSON(MNRLNetwork &net) {
			Document d(kObjectType);
			
			Value n(kArrayType);

			for(auto &kv : net.getNodes()) {
				Value j;
				
				switch(kv.second->getNodeType()) {
					case MNRLDefs::NodeType::NODE:
					j = JSONWriter::toJSON(dynamic_cast<MNRLNode*>(kv.second),d );
					break;
					case MNRLDefs::NodeType::STATE:
					j = JSONWriter::toJSON(dynamic_cast<MNRLState*>(kv.second), d);
					break;
					case MNRLDefs::NodeType::HSTATE:
					j = JSONWriter::toJSON(dynamic_cast<MNRLHState*>(kv.second), d);
					break;
					case MNRLDefs::NodeType::BOOLEAN:
					j = JSONWriter::toJSON(dynamic_cast<MNRLBoolean*>(kv.second), d);
					break;
					case MNRLDefs::NodeType::UPCOUNTER:
					j = JSONWriter::toJSON(dynamic_cast<MNRLUpCounter*>(kv.second), d);
					break;
				}
				n.PushBack(j, d.GetAllocator());
			}

			d.AddMember("id", StringRef(net.getId().c_str()), d.GetAllocator());
			d.AddMember("nodes", n, d.GetAllocator());
			return d;
		}
	private:
		static Value toJSON(MNRLNode *n, Document &d) {
			// convert to input port definitions
			Value iDefs(kArrayType);
			for(auto &kv : n->getInputConnections()) {
				Value obj(kObjectType);
				obj.AddMember("portId", Value(StringRef(kv.first.c_str())), d.GetAllocator());
				obj.AddMember("width", kv.second.getWidth(), d.GetAllocator());
				iDefs.PushBack(obj, d.GetAllocator());
			}
			
			// convert to output port definitions
			Value oDefs(kArrayType);
			for(auto &kv : n->getOutputConnections()) {
				// get all of the connections
				Value mnrl_conn(kArrayType);
				for(auto &np : kv.second.getConnections()) {
					Value obj(kObjectType);
					obj.AddMember("id", Value(StringRef(np.first.c_str())), d.GetAllocator());
					obj.AddMember("portId", Value(StringRef(np.second.c_str())), d.GetAllocator());
					mnrl_conn.PushBack(obj, d.GetAllocator());
				}
				
				Value obj(kObjectType);
				obj.AddMember("portId", Value(StringRef(kv.first.c_str())), d.GetAllocator());
				obj.AddMember("width", kv.second.getWidth(), d.GetAllocator());
				obj.AddMember("activate", mnrl_conn, d.GetAllocator());
				oDefs.PushBack(obj, d.GetAllocator());
			}
			
			Value node(kObjectType);
			node.AddMember("id", Value(StringRef(n->getId().c_str())), d.GetAllocator());
			node.AddMember("report", n->getReport(), d.GetAllocator());
			std::string enable = MNRLDefs::toMNRLEnable(n->getEnable());
			node.AddMember("enable", Value(enable.c_str(), d.GetAllocator()), d.GetAllocator());
			node.AddMember("outputDefs", oDefs, d.GetAllocator());
			node.AddMember("inputDefs", iDefs, d.GetAllocator());
			
			Value attrs(kObjectType);
			for(auto &kv : n->getAttributes()) {
				attrs.AddMember(Value(StringRef(kv.first.c_str())), Value(StringRef(kv.second.c_str())), d.GetAllocator());
			}
			node.AddMember("attributes", attrs, d.GetAllocator());
			
			switch(n->getReportEnable()) {
				case MNRLDefs::ReportEnableType::ENABLE_ON_LAST:
				node.AddMember("reportEnable", "onLast", d.GetAllocator());
				break;
				default:
				break;
			}
			
			return node;
		}
		static Value toJSON(MNRLState *s, Document &d) {
			Value mapping = toJSON(dynamic_cast<MNRLNode*>(s), d);
			
			// insert the type
			mapping.AddMember("type", "state", d.GetAllocator());

			Value symbolSet(kObjectType);
			for(auto &st : s->getOutputSymbols()) {
				// add the symbolSet for the given port
				symbolSet.AddMember(Value(StringRef(st.first.c_str())), Value(StringRef(st.second.c_str())), d.GetAllocator());
			}
			
			// insert the symbolSets
			mapping["attributes"].AddMember("symbolSet", symbolSet, d.GetAllocator());
			
			// insert reportId
			mapping["attributes"].AddMember("reportId", JSONWriter::toJSON(s->getReportId(), d), d.GetAllocator());
			
			// insert latched
			mapping["attributes"].AddMember("latched", s->getLatched(), d.GetAllocator());
			
			return mapping;
		}
		static Value toJSON(MNRLHState *s, Document &d) {
			Value mapping = toJSON(dynamic_cast<MNRLNode*>(s), d);
			
			// insert the type
			mapping.AddMember("type", "hState", d.GetAllocator());
			
			// insert the symbolSets
			mapping["attributes"].AddMember("symbolSet", Value(StringRef(s->getSymbolSet().c_str())), d.GetAllocator());
			
			// insert reportId
			mapping["attributes"].AddMember("reportId", JSONWriter::toJSON(s->getReportId(), d), d.GetAllocator());
			
			// insert latched
			mapping["attributes"].AddMember("latched", s->getLatched(), d.GetAllocator());
			
			return mapping;
		}
		static Value toJSON(MNRLBoolean *b, Document &d) {
			Value mapping = toJSON(dynamic_cast<MNRLNode*>(b), d);
			
			// insert the type
			mapping.AddMember("type", "boolean", d.GetAllocator());
			
			// insert reportId
			mapping["attributes"].AddMember("reportId", JSONWriter::toJSON(b->getReportId(), d), d.GetAllocator());
			
			// insert mode
			mapping["attributes"].AddMember("gateType", Value(MNRLDefs::toMNRLBooleanMode(b->getMode()).c_str(), d.GetAllocator()), d.GetAllocator());
			
			return mapping;
		}
		static Value toJSON(MNRLUpCounter *c, Document &d) {
			Value mapping = toJSON(dynamic_cast<MNRLNode*>(c), d);
			
			// insert the type
			mapping.AddMember("type", "upCounter", d.GetAllocator());
			
			// insert the threshold
			mapping.AddMember("threshold", c->getThreshold(), d.GetAllocator());
			
			// insert reportId
			mapping["attributes"].AddMember("reportId", JSONWriter::toJSON(c->getReportId(), d), d.GetAllocator());
			
			// insert mode
			mapping.AddMember("mode", Value(MNRLDefs::toMNRLCounterMode(c->getMode()).c_str(), d.GetAllocator()), d.GetAllocator());
			
			return mapping;
		}
		
		static Value toJSON(MNRLReportId *r, Document &d) {
			switch(r->get_type()) {
				case MNRLDefs::ReportIdType::INT:
				return toJSON(dynamic_cast<MNRLReportIdInt*>(r), d);
				case MNRLDefs::ReportIdType::STRING:
				return toJSON(dynamic_cast<MNRLReportIdString*>(r), d);
				default:
				return Value();
			}
		}
		
		static Value toJSON(MNRLReportIdInt *r, Document &d) {
			return Value(r->getId());
		}
		
		static Value toJSON(MNRLReportIdString *r, Document &d) {
			return Value(StringRef(r->getId().c_str()));
		}
	};
}
