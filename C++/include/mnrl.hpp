// Kevin Angstadt
// angstadt {at} virginia.edu
// University of Virginia
//
// C++ objects for manipulating MNRL files

#ifndef MNRL_HPP
#define MNRL_HPP

#include "MNRLDefs.hpp"
#include "MNRLError.hpp"
#include "MNRLNetwork.hpp"
#include "MNRLNode.hpp"
#include "MNRLState.hpp"
#include "MNRLHState.hpp"
#include "MNRLUpCounter.hpp"
#include "MNRLBoolean.hpp"

namespace MNRL {
	namespace MNRLError {
		class MNRLError;
	}
	class MNRLNetwork;
	class MNRLDefs;
	class MNRLNode;
	class MNRLPort;
	class MNRLState;
	class MNRLHState;
	class MNRLUpCounter;
	class MNRLBoolean;

	MNRLNetwork loadMNRL(const std::string &filename);

}

#endif
