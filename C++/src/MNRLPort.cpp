/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLPort.cpp
 */

#include "MNRLPort.hpp"

using namespace std;
using namespace MNRL;

MNRLPort::MNRLPort(string id, int width) : id(id), width(width) {

}

MNRLPort::~MNRLPort() {

}

string MNRLPort::getId() {
	return id;
}

int MNRLPort::getWidth() {
	return width;
}

vector<pair<shared_ptr<MNRLNode>, shared_ptr<MNRLPort>>> MNRLPort::getConnections() {
	return connections;
}

void MNRLPort::addConnection(shared_ptr<MNRLNode> id, shared_ptr<MNRLPort> port) {
	connections.push_back(make_pair(id,port));
}

bool MNRLPort::deleteConnection(string node_to_delete, string port) {
	unsigned i;
	for(i=0; i<connections.size(); ++i) {
		if(connections[i].first->getId().compare(node_to_delete) == 0 &&
		   connections[i].second->getId().compare(port)) {
			connections.erase(connections.begin() + i);
			return true;
		}
	}
	return false;
}
