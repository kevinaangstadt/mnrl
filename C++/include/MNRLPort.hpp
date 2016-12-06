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

#include "mnrl.hpp"

namespace MNRL {
    class MNRLPort {
        public:
            MNRLPort(
                std::string id,
                unsigned int width
            );
            ~MNRLPort();

            // accessor methods
            std::string getId();
            unsigned int getWidth();
            std::vector<std::pair<std::shared_ptr<MNRL::MNRLNode>, std::shared_ptr<MNRL::MNRLPort>>> getConnections();

            void addConnection(std::shared_ptr<MNRL::MNRLNode> id, std::shared_ptr<MNRL::MNRLPort> port);
            bool deleteConnection(std::string id, std::string port);


        private:
            std::string id;
            unsigned int width;
            std::vector<std::pair<std::shared_ptr<MNRL::MNRLNode>, std::shared_ptr<MNRL::MNRLPort>>> connections;
    };
}

#endif
