// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLPort Object

#ifndef MNRLPORT_HPP
#define MNRLPORT_HPP

#include <string>
#include <vector>
#include <utility>
#include <memory>
#include <map>

#include "MNRLDefs.hpp"

namespace MNRL {
	class MNRLNode;
	class MNRLPort;

	typedef std::map<std::string, std::shared_ptr<MNRL::MNRLPort>> port_map;
	typedef std::vector<std::pair<std::shared_ptr<MNRL::MNRLNode>, std::shared_ptr<MNRL::MNRLPort>>> port_conns;


    class MNRLPort {
        public:
            MNRLPort(
                std::string id,
                int width
            );
            ~MNRLPort();

            // accessor methods
            std::string getId();
            int getWidth();
            MNRL::port_conns getConnections();

            void addConnection(std::shared_ptr<MNRL::MNRLNode> id, std::shared_ptr<MNRL::MNRLPort> port);
            bool deleteConnection(std::string id, std::string port);


        private:
            std::string id;
            int width;
            MNRL::port_conns connections;
    };
}

#endif
