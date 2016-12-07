// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLNetwork Object

#ifndef MNRLDEFS_HPP
#define MNRLDEFS_HPP

#include <string>

namespace MNRL {
    class MNRLDefs {
        public:
            enum EnableType {
                ENABLE_ALWAYS,
                ENABLE_ON_ACTIVATE_IN,
                ENABLE_ON_START_AND_ACTIVATE_IN,
                ENABLE_ON_LAST
            };

            enum CounterMode {
                TRIGGER_ON_THRESHOLD,
                HIGH_ON_THRESHOLD,
                ROLLOVER_ON_THRESHOLD
            };

            enum BooleanMode {
            	AND,
				OR,
				NOR,
				NOT,
				NAND
            };

            static const std::string STATE_INPUT,
            						 H_STATE_INPUT,
									 H_STATE_OUTPUT,
									 UP_COUNTER_OUTPUT,
									 BOOLEAN_OUTPUT,
									 UP_COUNTER_COUNT,
									 UP_COUNTER_RESET;


            static unsigned int BooleanToPort(const BooleanMode b);
            static std::string toMNRLEnable(const EnableType e);
    };
}

#endif
