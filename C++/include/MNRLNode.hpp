// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLNetwork Object

#ifndef MNRLNODE_HPP
#define MNRLNODE_HPP

#include <map>
#include <string>
#include <vector>
#include <tuple>
#include <json11.hpp>
#include "MNRLPort.hpp"


namespace MNRL {
    class MNRLNode {
        public:
            MNRLNode(
                std::string id,
                MNRLDefs::EnableType enable,
                bool report,
                std::vector<MNRL::MNRLPort> inputDefs,
                std::vector<MNRL::MNRLPort> outputDefs,
                std::map<std::string, json11::Json> attributes
            );
            ~MNRLNode();
            json11::Json to_json();

            std::map<std::string, MNRL::MNRLPort> getOutputConnections();
            std::map<std::string, MNRL::MNRLPort> getInputConnections();

            std::string getId();
            bool getReport();
            MNRLDefs::EnableType getEnable();

            void setId(std::string new_id);
            void setReport(bool r);
            void setEnable(MNRLDefs::EnableType e);


        protected:
            std::string id;
            bool report;
            MNRLDefs::EnableType enable;
            std::map<std::string, MNRL::MNRLPort> inputDefs;
            std::map<std::string, MNRL::MNRLPort> outputDefs;
            std::map<std::string, json11::Json> attributes;

        private:
            std::map<std::string, MNRL::MNRLPort> validate_ports(std::vector<MNRL::MNRLPort> portdef);
    };


}

#endif
