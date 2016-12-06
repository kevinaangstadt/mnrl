/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLPort.cpp
 */

#import "MNRLPort.hpp"

MNRL::MNRLPort(std::string id, unsigned int width) : MNRL::MNRLPort::id(id), MNRL::MNRLPort::width(width) {

}

MNRL::MNRLPort::~MNRLPort() {

}

std::string MNRL::MNRLPort::getId() {
	return id;
}

unsigned int MNRL::MNRLPort::getWidth() {
	return width;
}

std::vector<std::pair<std::shared_ptr<MNRL::MNRLNode>, std::shared_ptr<MNRL::MNRLPort>>> MNRL::MNRLPort::getConnections() {
	return connections;
}

void MNRL::MNRLPort::addConnection(std::shared_ptr<MNRL::MNRLNode> id, std::shared_ptr<MNRL::MNRLPort> port) {
	connections.push_back(std::make_tuple(id,port));
}

bool MNRL::MNRLPort::deleteConnection(std::string node_to_delete, std::string port) {
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
