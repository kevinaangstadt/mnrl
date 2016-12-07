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
	    with open("mnrl-schema.json", "r") as s:
	        schema = json.load(s)
	    with open(filename, "r") as f:
	        json_string = f.read()

	        try:
	            jsonschema.validate(json.loads(json_string),schema)
	        except jsonschema.exceptions.ValidationError as e:
	            print "ERROR:", e
	            return None

	        # parse into MNRL
	        d = MNRLDecoder()
	        return d.decode(json_string)
}

#endif
