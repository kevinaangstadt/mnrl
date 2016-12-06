// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLPort Object

#ifndef MNRLPORT_HPP
#define MNRLPORT_HPP

#include <string>
#include <vector>
#include <tuple>
#include <memory>

#include "MNRLDefs.hpp"
#include "MNRLNode.hpp"

namespace MNRL {
	class MNRLNode;
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
            std::vector<std::pair<std::shared_ptr<MNRL::MNRLNode>, std::shared_ptr<MNRL::MNRLPort>>> getConnections();

            void addConnection(std::shared_ptr<MNRL::MNRLNode> id, std::shared_ptr<MNRL::MNRLPort> port);
            bool deleteConnection(std::string id, std::string port);


        private:
            std::string id;
            int width;
            std::vector<std::pair<std::shared_ptr<MNRL::MNRLNode>, std::shared_ptr<MNRL::MNRLPort>>> connections;
    };
}

#endif
