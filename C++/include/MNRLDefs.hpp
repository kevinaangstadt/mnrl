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

            const std::string STATE_INPUT = "i";
            const std::string H_STATE_INPUT = STATE_INPUT;

            const std::string H_STATE_OUTPUT = "o";
            const std::string UP_COUNTER_OUTPUT = H_STATE_OUTPUT;
            const std::string BOOLEAN_OUTPUT = H_STATE_OUTPUT;


            static unsigned int BooleanToPort(const BooleanMode b);
            static std::string toMNRLEnable(const EnableType e);
    };
}

#endif
