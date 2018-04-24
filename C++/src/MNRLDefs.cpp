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
	case BooleanMode::AND:
	case BooleanMode::OR:
	case BooleanMode::NOR:
	case BooleanMode::NOT:
	case BooleanMode::NAND:
		return 1;
	default:
		// not reached
		return 0;
	}
}

string MNRLDefs::toMNRLEnable(const MNRLDefs::EnableType e) {
	switch (e) {
	case EnableType::ENABLE_ALWAYS:
		return "always";
	case EnableType::ENABLE_ON_ACTIVATE_IN:
		return "onActivateIn";
	case EnableType::ENABLE_ON_START_AND_ACTIVATE_IN:
		return "onStartAndActivateIn";
	case EnableType::ENABLE_ON_LAST:
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

MNRLDefs::ReportEnableType MNRLDefs::fromMNRLReportEnable(const string r) {
	if(r.compare("always") == 0) 
		return MNRLDefs::ReportEnableType::ENABLE_ALWAYS;
	else if(r.compare("onLast") == 0) 
		return MNRLDefs::ReportEnableType::ENABLE_ON_LAST;
	else
		throw MNRLError::ReportEnableError(r);
}

string MNRLDefs::toMNRLCounterMode(const MNRLDefs::CounterMode c) {
	switch(c) {
	case CounterMode::TRIGGER_ON_THRESHOLD:
		return "trigger";
	case CounterMode::HIGH_ON_THRESHOLD:
		return "high";
	case CounterMode::ROLLOVER_ON_THRESHOLD:
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
	case BooleanMode::AND:
		return "and";
	case BooleanMode::OR:
		return "or";
	case BooleanMode::NOR:
		return "nor";
	case BooleanMode::NOT:
		return "not";
	case BooleanMode::NAND:
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

