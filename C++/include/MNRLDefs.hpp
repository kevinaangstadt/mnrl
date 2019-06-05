// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLNetwork Object

#ifndef MNRLDEFS_HPP
#define MNRLDEFS_HPP

#include <string>
#include "MNRLError.hpp"

namespace MNRL {
  class MNRLDefs {
  public:
    enum class EnableType {
      ENABLE_ALWAYS,
      ENABLE_ON_ACTIVATE_IN,
      ENABLE_ON_START_AND_ACTIVATE_IN,
      ENABLE_ON_LAST
    };
    
    enum class ReportEnableType {
      ENABLE_ALWAYS,
      ENABLE_ON_LAST
    };
    
    enum class CounterMode {
      TRIGGER_ON_THRESHOLD,
      HIGH_ON_THRESHOLD,
      ROLLOVER_ON_THRESHOLD
    };
    
    enum class BooleanMode {
      AND,
      OR,
      NOR,
      NOT,
      NAND
    };
    
    enum class NodeType {
      NODE,
      STATE,
      HSTATE,
      BOOLEAN,
      UPCOUNTER
    };
    
    enum class ReportIdType {
      NONE,
      INT,
      STRING
    };
    
    static const std::string STATE_INPUT,
                             H_STATE_INPUT,
                             H_STATE_OUTPUT,
                             UP_COUNTER_OUTPUT,
                             BOOLEAN_OUTPUT,
                             UP_COUNTER_COUNT,
                             UP_COUNTER_RESET;
    
    static unsigned int BooleanToPort(const BooleanMode b) {
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
    
    static std::string toMNRLEnable(const EnableType e) {
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
    
    static EnableType fromMNRLEnable(const std::string &e) {
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
    
    static ReportEnableType fromMNRLReportEnable(const std::string &r) {
    	if(r.compare("always") == 0) 
    		return MNRLDefs::ReportEnableType::ENABLE_ALWAYS;
    	else if(r.compare("onLast") == 0) 
    		return MNRLDefs::ReportEnableType::ENABLE_ON_LAST;
    	else
    		throw MNRLError::ReportEnableError(r);
    }
    
    static std::string toMNRLCounterMode(const CounterMode c) {
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
    
    static CounterMode fromMNRLCounterMode(const std::string &c) {
    	if(c.compare("trigger") == 0)
    		return MNRLDefs::CounterMode::TRIGGER_ON_THRESHOLD;
    	else if(c.compare("high") == 0)
    		return MNRLDefs::CounterMode::HIGH_ON_THRESHOLD;
    	else if(c.compare("rollover") == 0)
    		return MNRLDefs::CounterMode::ROLLOVER_ON_THRESHOLD;
    	else
    		throw MNRLError::UpCounterModeError(c);
    }

    static std::string toMNRLBooleanMode(const BooleanMode b) {
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

    static BooleanMode fromMNRLBooleanMode(const std::string &b) {
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
  };
}



#endif
