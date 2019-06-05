// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLNetwork Object

#ifndef MNRLNODE_HPP
#define MNRLNODE_HPP

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>
#include "MNRLDefs.hpp"
#include "MNRLError.hpp"


namespace MNRL {
	class MNRLPort;
	class MNRLNode;
	
	using port_def = std::vector<MNRLPort>;
	using port_map = std::map<std::string, MNRLPort>;
	using port_conns = std::set<std::pair<std::string, std::string>>;
	
	class MNRLPort {
	public:
		MNRLPort(
			std::string id,
			int width
		) : id(id.c_str()), width(width) {}
		MNRLPort(
			char id[],
			int width
		) : id(std::string(id)), width(width) {}
		explicit MNRLPort(const MNRLPort &other) : id(other.id), width(other.width), connections(other.connections){}
		MNRLPort() : id(""), width(0) {}
		~MNRLPort() {}
		
		MNRLPort &operator=(const MNRLPort &other) {
			MNRLPort tmp(other);
			std::swap(id, tmp.id);
			std::swap(width, tmp.width);
			std::swap(connections, tmp.connections);
			return *this;
		}
		
		// accessor methods
		std::string getId() { return id; }
		int getWidth() const { return width; }
		const port_conns &getConnections() const { return connections; }
		
		void addConnection(const std::string &id, const std::string &port) {
			connections.emplace(id, port);
		}
		bool deleteConnection(const std::string &id, const std::string &port) {
			return connections.erase({id,port}) != 0;
		}
		
		
	private:
		std::string id;
		int width;
		port_conns connections;
	};
	
	class MNRLNode {
	public:
		MNRLNode(
			std::string id,
			MNRLDefs::EnableType enable,
			bool report,
			port_def inputDefs,
			port_def outputDefs,
			std::map<std::string,std::string> attributes,
			MNRLDefs::ReportEnableType report_enable = MNRLDefs::ReportEnableType::ENABLE_ALWAYS
		) : id(id),
			enable(enable),
			report(report),
			inputDefs( MNRLNode::validate_ports(inputDefs) ),
			outputDefs( MNRLNode::validate_ports(outputDefs) ),
			attributes(attributes),
			reportEnable(report_enable) {} 
		
		MNRLNode(MNRLNode &other) :
			id(other.id),
			enable(other.enable),
			report(other.report),
			inputDefs(MNRLNode::validate_ports(other.inputDefs)),
			outputDefs(MNRLNode::validate_ports(other.outputDefs)),
			attributes(other.attributes),
			reportEnable(other.reportEnable) {}
		virtual ~MNRLNode() {}
		
		virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::NODE; }
		
		const port_map& getOutputConnections() const { return outputDefs; }
		const port_map& getInputConnections() const { return inputDefs; }
		
		MNRLPort& getOutputPort(std::string &portId) {
			auto it = outputDefs.find(portId);
			if (it != outputDefs.end()) {
				return it->second;
			} else {
				throw MNRLError::UnknownPort(portId);
			}
		}
		MNRLPort& getInputPort(std::string &portId) {
			auto it = inputDefs.find(portId);
			if (it != inputDefs.end()) {
				return it->second;
			} else {
				throw MNRLError::UnknownPort(portId);
			}
		}
		
		const std::string& getId() const { return id; }
		bool getReport() { return report; }
		MNRLDefs::EnableType getEnable() { return enable; }
		MNRLDefs::ReportEnableType getReportEnable() { return reportEnable; }
		std::map<std::string,std::string>& getAttributes() { return attributes; }
		
		void setId(std::string& new_id) { id = new_id; }
		void setReport(bool r) { report = r; }
		void setEnable(MNRLDefs::EnableType e) { enable = e; }
		void setReportEnable(MNRLDefs::ReportEnableType r) { reportEnable = r; }
		
	protected:
		std::string id;
		bool report;
		MNRLDefs::ReportEnableType reportEnable;
		MNRLDefs::EnableType enable;
		port_map inputDefs;
		port_map outputDefs;
		std::map<std::string,std::string> attributes;
		
	private:
		static port_map validate_ports(port_def &portdef) {
			port_map ports;
			
			for (MNRLPort &p : portdef) {
				ports[p.getId()] = p ;
			}
			
			return ports;
		}
		
		static port_map validate_ports(port_map &pm) {
			port_map ports;
			
			for (auto &kv : pm) {
				ports[kv.first] = kv.second;
			}
			
			return ports;
		}
	};
}

#endif
