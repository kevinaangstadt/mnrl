/*
 * test MNRL API by reading in MNRL file and print all node ids
 */
#include <iostream>
#include "mnrl.hpp"
using namespace std;
int main(int argc, char *argv[]) {
	if(argc != 2) {
		cout << "usage: readwrite infile" << endl;
		return 1;
	}

	shared_ptr<MNRL::MNRLNetwork> m = MNRL::loadMNRL(string(argv[1]));
	for(auto &node : m->getNodes()) {
		cout << node.second->getId() << endl;
	}
	return 0;
}
