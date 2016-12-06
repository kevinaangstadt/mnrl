// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLNetwork Object

#ifndef MNRLDEFS_HPP
#define MNRLDEFS_HPP

#include <string>
#include "mnrl.hpp"

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

            static unsigned int BooleanToPort(const BooleanMode b);
            static std::string toMNRLEnable(const EnableType e);
    };
}

#endif
