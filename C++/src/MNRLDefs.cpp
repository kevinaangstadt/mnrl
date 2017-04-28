/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLDefs.cpp
 */

#include "MNRLDefs.hpp"
#include "MNRLError.hpp"

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

MNRLDefs::EnableType MNRLDefs::fromMNRLEnable(const string e) {
	if(e.compare("onActivateIn") == 0 )
		return MNRLDefs::EnableType::ENABLE_ON_ACTIVATE_IN;
	else if(e.compare("onStartAndActivateIn") == 0 )
		return MNRLDefs::EnableType::ENABLE_ON_START_AND_ACTIVATE_IN;
	else if(e.compare("onLast") == 0)
		return MNRLDefs::EnableType::ENABLE_ON_LAST;
	else if(e.compare("always") == 0)
		return MNRLDefs::EnableType::ENABLE_ALWAYS;
	else
		throw MNRLError::EnableError(e);
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

MNRLDefs::CounterMode MNRLDefs::fromMNRLCounterMode(const string c) {
	if(c.compare("trigger") == 0)
		return MNRLDefs::CounterMode::TRIGGER_ON_THRESHOLD;
	else if(c.compare("high") == 0)
		return MNRLDefs::CounterMode::HIGH_ON_THRESHOLD;
	else if(c.compare("rollover") == 0)
		return MNRLDefs::CounterMode::ROLLOVER_ON_THRESHOLD;
	else
		throw MNRLError::UpCounterModeError(c);
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

MNRLDefs::BooleanMode MNRLDefs::fromMNRLBooleanMode(const string b) {
	if(b.compare("and") == 0)
		return MNRLDefs::BooleanMode::AND;
	else if(b.compare("or") == 0)
		return MNRLDefs::BooleanMode::OR;
	else if(b.compare("nor") == 0)
		return MNRLDefs::BooleanMode::NOR;
	else if(b.compare("not") == 0)
		return MNRLDefs::BooleanMode::NOT;
	else if(b.compare("nand") == 0)
		return MNRLDefs::BooleanMode::NAND;
	else
		throw MNRLError::InvalidGateType(b);
}



const string MNRLDefs::STATE_INPUT = "i";
const string MNRLDefs::H_STATE_INPUT = "i";
const string MNRLDefs::H_STATE_OUTPUT = "o";
const string MNRLDefs::UP_COUNTER_OUTPUT = "o";
const string MNRLDefs::BOOLEAN_OUTPUT = "o";
const string MNRLDefs::UP_COUNTER_COUNT = "cnt";
const string MNRLDefs::UP_COUNTER_RESET = "rst";
const string MNRLDefs::PFP_STATE_INPUT = "i";
const string MNRLDefs::PFP_STATE_OUTPUT = "o";

