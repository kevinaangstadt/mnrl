/*
 * test MNRL API by reading in MNRL file and writing it back out
 */
#include <iostream>
#include "mnrl.hpp"
using namespace std;
int main(int argc, char *argv[]) {
	if(argc != 3) {
		cout << "usage: readwrite infile outfile" << endl;
		return 1;
	}

	MNRL::MNRLNetwork m = MNRL::loadMNRL(string(argv[1]));
	m.exportToFile(string(argv[2]));
	return 0;
}
