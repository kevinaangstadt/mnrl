/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLDefs.cpp
 */

#include "MNRLDefs.hpp"

using namespace std;
using namespace MNRL;

unsigned int MNRLDefs::BooleanToPort(const BooleanMode b) {
	switch(b){
	case AND:
	case OR:
	case NOR:
	case NOT:
	case NAND:
		return 1;
	default:
		// not reached
		return 0;
	}
}

string MNRLDefs::toMNRLEnable(const MNRLDefs::EnableType e) {
	switch (e) {
	case ENABLE_ALWAYS:
		return "always";
	case ENABLE_ON_ACTIVATE_IN:
		return "onActivateIn";
	case ENABLE_ON_START_AND_ACTIVATE_IN:
		return "onStartAndActivateIn";
	case ENABLE_ON_LAST:
		return "onLast";
	default:
		// should not be reached
		return "";
	}
}

string MNRLDefs::toMNRLCounterMode(const MNRLDefs::CounterMode c) {
	switch(c) {
	case TRIGGER_ON_THRESHOLD:
		return "trigger";
	case HIGH_ON_THRESHOLD:
		return "high";
	case ROLLOVER_ON_THRESHOLD:
		return "rollover";
	default:
		//should not be reached
		return "";
	}
}

string MNRLDefs::toMNRLBooleanMode(const BooleanMode b) {
	switch(b){
	case AND:
		return "and";
	case OR:
		return "or";
	case NOR:
		return "nor";
	case NOT:
		return "not";
	case NAND:
		return "nand";
	default:
		// not reached
		return "";
	}
}



const string MNRLDefs::STATE_INPUT = "i";
const string MNRLDefs::H_STATE_INPUT = "i";
const string MNRLDefs::H_STATE_OUTPUT = "o";
const string MNRLDefs::UP_COUNTER_OUTPUT = "o";
const string MNRLDefs::BOOLEAN_OUTPUT = "o";
const string MNRLDefs::UP_COUNTER_COUNT = "cnt";
const string MNRLDefs::UP_COUNTER_RESET = "rst";

