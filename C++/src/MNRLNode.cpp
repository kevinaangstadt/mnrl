/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLNode.cpp
 */

#include "MNRLNode.hpp"

#include <iostream>

using namespace std;
using namespace MNRL;

MNRLNode::MNRLNode(
	string id,
	MNRLDefs::EnableType enable,
	bool report,
	port_def inputDefs,
	port_def outputDefs,
	shared_ptr<map<string,string>> attributes,
	MNRLDefs::ReportEnableType report_enable
) : id(id),
	enable(enable),
	report(report),
	inputDefs( MNRLNode::validate_ports(inputDefs) ),
	outputDefs( MNRLNode::validate_ports(outputDefs) ),
	attributes(attributes),
	reportEnable(report_enable) {}

MNRLNode::~MNRLNode() {}

shared_ptr<port_map> MNRLNode::getOutputConnections(){ return outputDefs; }
shared_ptr<port_map> MNRLNode::getInputConnections(){ return inputDefs; }

shared_ptr<MNRLPort> MNRLNode::getOutputPort(string portId) {
	port_map::iterator it = outputDefs->find(portId);
	if (it == outputDefs->end()) {
		throw MNRLError::UnknownPort(portId);
	}
	return (*outputDefs)[portId];
}

shared_ptr<MNRLPort> MNRLNode::getInputPort(string portId) {
	port_map::iterator it = inputDefs->find(portId);
	if (it == inputDefs->end()) {
		cerr << "Valid ports are:" << endl;
		for (const auto p : *inputDefs){
			cerr << "    " << p.second->getId() << endl;
		}
		throw MNRLError::UnknownPort(portId);
	}
	return (*inputDefs)[portId];
}

string MNRLNode::getId() { return id; }
bool MNRLNode::getReport() { return report; }
MNRLDefs::EnableType MNRLNode::getEnable(){ return enable; }
MNRLDefs::ReportEnableType MNRLNode::getReportEnable(){ return reportEnable; }

shared_ptr<map<string,string>> MNRLNode::getAttributes() { return attributes; }

void MNRLNode::setId(string new_id) {
	id = new_id;
}

void MNRLNode::setEnable(MNRLDefs::EnableType e) {
	enable = e;
}

void MNRLNode::setReport(bool b) {
	report = b;
}

void MNRLNode::setReportEnable(MNRLDefs::ReportEnableType r) {
	reportEnable = r;
}

