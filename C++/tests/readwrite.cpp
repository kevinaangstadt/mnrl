/*
 * test MNRL API by reading in MNRL file and writing it back out
 */
#include <iostream>
#include "mnrl.hpp"
using namespace std;
int main(int argc, char *argv[]) {
	if(argc < 3 || argc > 4) {
		cout << "usage: readwrite infile outfile pretty|condensed" << endl;
		return 1;
	}

	MNRL::MNRLNetwork m = MNRL::loadMNRL(string(argv[1]));
	m.exportToFile(string(argv[2]), argc == 4 ? argv[3][0] == 'p' : false);
	return 0;
}
