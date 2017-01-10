// Kevin Angstadt
// angstadt {at} virginia.edu
// University of Virginia
//
// C++ objects for manipulating MNRL files

#ifndef MNRL_HPP
#define MNRL_HPP

#include <memory>
#include <json11.hpp>

#include "MNRLDefs.hpp"
#include "MNRLError.hpp"
#include "MNRLNode.hpp"
#include "MNRLPort.hpp"
#include "MNRLState.hpp"
#include "MNRLHState.hpp"
#include "MNRLUpCounter.hpp"
#include "MNRLBoolean.hpp"

namespace MNRL {
	class MNRLNetwork;
	class MNRLDefs;
	class MNRLError;
	class MNRLNode;
	class MNRLPort;
	class MNRLState;
	class MNRLHState;
	class MNRLUpCounter;
	class MNRLBoolean;

	std::shared_ptr<MNRLNetwork> loadMNRL(std::string filename);

}

#endif
