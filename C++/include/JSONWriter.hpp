// Kevin Angstadt
// angstadt {at} virginia.edu
//
// Functions for Creating JSON from various MNRL objects

#include <json11.hpp>
#include <vector>
#include <memory>

#include "MNRLNetwork.hpp"
#include "MNRLNode.hpp"
#include "MNRLState.hpp"
#include "MNRLHState.hpp"
#include "MNRLBoolean.hpp"
#include "MNRLUpCounter.hpp"
#include "MNRLHPDState.hpp"

namespace MNRL {
class JSONWriter {
public:
	static json11::Json toJSON(MNRLNetwork net) {
		std::vector<json11::Json> n;
		for(auto &kv : net.getNodes()) {
			json11::Json j;

			switch(kv.second->getNodeType()) {
			case MNRLDefs::NodeType::NODE:
				j = JSONWriter::toJSON(std::dynamic_pointer_cast<MNRLNode>(kv.second));
				break;
			case MNRLDefs::NodeType::STATE:
				j = JSONWriter::toJSON(std::dynamic_pointer_cast<MNRLState>(kv.second));
				break;
			case MNRLDefs::NodeType::HSTATE:
				j = JSONWriter::toJSON(std::dynamic_pointer_cast<MNRLHState>(kv.second));
				break;
			case MNRLDefs::NodeType::BOOLEAN:
				j = JSONWriter::toJSON(std::dynamic_pointer_cast<MNRLBoolean>(kv.second));
				break;
			case MNRLDefs::NodeType::UPCOUNTER:
				j = JSONWriter::toJSON(std::dynamic_pointer_cast<MNRLUpCounter>(kv.second));
				break;
			case MNRLDefs::NodeType::HPDSTATE:
				j = JSONWriter::toJSON(std::dynamic_pointer_cast<MNRLHPDState>(kv.second));
			}
			n.push_back(j);
		}

		return json11::Json::object {
			{"id", net.getId()},
			{"nodes", n }
		};
	}
private:
	static json11::Json toJSON(std::shared_ptr<MNRLNode> n) {
		// convert to input port definitions
		std::vector<json11::Json::object> iDefs;
		for(auto &kv : *(n->getInputConnections())) {
			iDefs.push_back(json11::Json::object {
				{ "portId", kv.first },
				{ "width", kv.second->getWidth() }
			});
		}

		// convert to output port definitions
		std::vector<json11::Json::object> oDefs;
		for(auto &kv : *(n->getOutputConnections())) {
			// get all of the connections
			port_conns conn = kv.second->getConnections();
			std::vector<json11::Json::object> mnrl_conn;
			for(std::pair<std::shared_ptr<MNRLNode>, std::shared_ptr<MNRLPort>> &np : conn) {
				mnrl_conn.push_back(json11::Json::object {
					{ "id", np.first->getId() },
					{ "portId", np.second->getId() }
				});
			}

			oDefs.push_back(json11::Json::object {
				{ "portId", kv.first },
				{ "width", kv.second->getWidth() },
				{ "activate", mnrl_conn }
			});
		}

		return json11::Json::object {
			{ "id", n->getId() },
			{ "report", n->getReport() },
			{ "enable", MNRLDefs::toMNRLEnable(n->getEnable()) },
			{ "outputDefs", json11::Json(oDefs) },
			{ "inputDefs", json11::Json(iDefs) },
			{ "attributes", json11::Json(*(n->getAttributes())) }
		};
	}
	static json11::Json toJSON(std::shared_ptr<MNRLState> s) {
		json11::Json parent = toJSON(std::dynamic_pointer_cast<MNRLNode>(s));

		// we know that this is an obj
		std::map<std::string, json11::Json> mapping = parent.object_items();

		// insert the type
		mapping.insert(std::map<std::string, json11::Json>::value_type("type", json11::Json("state")));

		// get the attributes
		std::map<std::string, json11::Json> attrs = mapping["attributes"].object_items();

		std::map<std::string, json11::Json> symbolSet;
		for(auto &st : *(s->getOutputSymbols())) {
			// add the symbolSet for the given port
			symbolSet.insert(std::map<std::string,json11::Json>::value_type(st.first, json11::Json(st.second)));
		}

		// insert the symbolSets
		attrs.insert(std::map<std::string, json11::Json>::value_type("symbolSet", json11::Json(symbolSet)));

		// insert reportId
		attrs.insert(std::map<std::string, json11::Json>::value_type("reportId", JSONWriter::toJSON(s->getReportId())));

		// insert latched
		attrs.insert(std::map<std::string, json11::Json>::value_type("latched", json11::Json(s->getLatched())));

		// update the attributes
		mapping["attributes"] = json11::Json(attrs);

		return json11::Json(mapping);
	}
	static json11::Json toJSON(std::shared_ptr<MNRLHState> s) {
		json11::Json parent = JSONWriter::toJSON(std::dynamic_pointer_cast<MNRLNode>(s));

		// we know that this is an obj
		std::map<std::string, json11::Json> mapping = parent.object_items();

		// insert the type
		mapping.insert(std::map<std::string, json11::Json>::value_type("type", json11::Json("hState")));

		// get the attributes
		std::map<std::string, json11::Json> attrs = mapping["attributes"].object_items();

		// insert the symbolSets
		attrs.insert(std::map<std::string, json11::Json>::value_type("symbolSet", json11::Json(s->getSymbolSet())));

		// insert reportId
		attrs.insert(std::map<std::string, json11::Json>::value_type("reportId", JSONWriter::toJSON(s->getReportId())));

		// insert latched
		attrs.insert(std::map<std::string, json11::Json>::value_type("latched", json11::Json(s->getLatched())));

		// update the attributes
		mapping["attributes"] = json11::Json(attrs);

		return json11::Json(mapping);
	}
	static json11::Json toJSON(std::shared_ptr<MNRLBoolean> b) {
		json11::Json parent = JSONWriter::toJSON(std::dynamic_pointer_cast<MNRLNode>(b));

		// we know that this is an obj
		std::map<std::string, json11::Json> mapping = parent.object_items();

		// insert the type
		mapping.insert(std::map<std::string, json11::Json>::value_type("type", json11::Json("boolean")));

		// get the attributes
		std::map<std::string, json11::Json> attrs = mapping["attributes"].object_items();

		// insert reportId
		attrs.insert(std::map<std::string, json11::Json>::value_type("reportId", JSONWriter::toJSON(b->getReportId())));

		// insert latched
		attrs.insert(std::map<std::string, json11::Json>::value_type("mode", json11::Json(MNRLDefs::toMNRLBooleanMode(b->getMode()))));

		// update the attributes
		mapping["attributes"] = json11::Json(attrs);

		return json11::Json(mapping);
	}
	static json11::Json toJSON(std::shared_ptr<MNRLUpCounter> c) {
		json11::Json parent = JSONWriter::toJSON(std::dynamic_pointer_cast<MNRLNode>(c));

		// we know that this is an obj
		std::map<std::string, json11::Json> mapping = parent.object_items();

		// insert the type
		mapping.insert(std::map<std::string, json11::Json>::value_type("type", json11::Json("upCounter")));

		// get the attributes
		std::map<std::string, json11::Json> attrs = mapping["attributes"].object_items();

		// insert the symbolSets
		attrs.insert(std::map<std::string, json11::Json>::value_type("threshold", json11::Json(c->getThreshold())));

		// insert reportId
		attrs.insert(std::map<std::string, json11::Json>::value_type("reportId", JSONWriter::toJSON(c->getReportId())));

		// insert latched
		attrs.insert(std::map<std::string, json11::Json>::value_type("mode", json11::Json(MNRLDefs::toMNRLCounterMode(c->getMode()))));

		// update the attributes
		mapping["attributes"] = json11::Json(attrs);

		return json11::Json(mapping);
	}
	static json11::Json toJSON(std::shared_ptr<MNRLHPDState> s) {
		json11::Json parent = JSONWriter::toJSON(std::dynamic_pointer_cast<MNRLNode>(s));

		// we know that this is an obj
		std::map<std::string, json11::Json> mapping = parent.object_items();

		// insert the type
		mapping.insert(std::map<std::string, json11::Json>::value_type("type", json11::Json("hPDState")));

		// get the attributes
		std::map<std::string, json11::Json> attrs = mapping["attributes"].object_items();

		// insert the symbolSets
		if(!(s->isEpsilonInput()))
			attrs.insert(std::map<std::string, json11::Json>::value_type("symbolSet", json11::Json(s->getSymbolSet())));

		// insert the stackSet
		attrs.insert(std::map<std::string, json11::Json>::value_type("stackSet", json11::Json(s->getStackSet())));
		
		// insert the stackPush
		if(s->doesStackPush())
			attrs.insert(std::map<std::string, json11::Json>::value_type("stackPush", json11::Json(s->getPushSymbol())));
		
		// insert the stackPop
		attrs.insert(std::map<std::string, json11::Json>::value_type("stackPop", json11::Json((int)s->getPop())));

		// insert reportId
		attrs.insert(std::map<std::string, json11::Json>::value_type("reportId", JSONWriter::toJSON(s->getReportId())));

		// update the attributes
		mapping["attributes"] = json11::Json(attrs);

		return json11::Json(mapping);
	}

	static json11::Json toJSON(std::shared_ptr<MNRLReportId> r) {
		switch(r->get_type()) {
		case MNRLDefs::ReportIdType::INT:
			return toJSON(std::dynamic_pointer_cast<MNRLReportIdInt>(r));
		case MNRLDefs::ReportIdType::STRING:
			return toJSON(std::dynamic_pointer_cast<MNRLReportIdString>(r));
		default:
			return json11::Json();
		}
	}

	static json11::Json toJSON(std::shared_ptr<MNRLReportIdInt> r) {
		return json11::Json(r->getId());
	}

	static json11::Json toJSON(std::shared_ptr<MNRLReportIdString> r) {
		return json11::Json(r->getId());
	}
};
}
