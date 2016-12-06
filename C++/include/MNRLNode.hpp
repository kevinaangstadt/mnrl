// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLNetwork Object

#ifndef MNRLNODE_HPP
#define MNRLNODE_HPP

#include <map>
#include <memory>
#include <string>
#include <vector>
#include <json11.hpp>
#include "MNRLDefs.hpp"
#include "MNRLPort.hpp"


namespace MNRL {
	class MNRLDefs;
	class MNRLPort;
    class MNRLNode {
        public:
			MNRLNode(
                std::string id,
                MNRL::MNRLDefs::EnableType enable,
                bool report,
                std::vector<std::shared_ptr<MNRL::MNRLPort>> inputDefs,
                std::vector<std::shared_ptr<MNRL::MNRLPort>> outputDefs,
                std::map<std::string, std::shared_ptr<json11::Json>> attributes
            );
            virtual ~MNRLNode();
            virtual json11::Json to_json();

            std::shared_ptr<MNRL::port_map> getOutputConnections();
            std::shared_ptr<MNRL::port_map> getInputConnections();

            std::string getId();
            bool getReport();
            MNRL::MNRLDefs::EnableType getEnable();

            void setId(std::string new_id);
            void setReport(bool r);
            void setEnable(MNRL::MNRLDefs::EnableType e);


        protected:
            std::string id;
            bool report;
            MNRL::MNRLDefs::EnableType enable;
            std::map<std::string, *MNRL::MNRLPort> *inputDefs;
            std::map<std::string, *MNRL::MNRLPort> *outputDefs;
            std::map<std::string, *json11::Json> attributes;

        private:
            std::shared_ptr<MNRL::port_map> validate_ports(std::vector<std::shared_ptr<MNRL::MNRLPort>> &portdef);
    };


}

#endif
