/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLDefs.cpp
 */

#include "MNRLDefs.hpp"


unsigned int MNRL::MNRLDefs::BooleanToPort(const BooleanMode b) {
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

std::string MNRL::MNRLDefs::toMNRLEnable(const MNRL::MNRLDefs::EnableType e) {
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


