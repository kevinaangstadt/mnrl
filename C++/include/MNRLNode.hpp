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
	typedef std::vector<std::shared_ptr<MNRL::MNRLPort>> port_def;

    class MNRLNode {
        public:
			MNRLNode(
                std::string id,
                MNRL::MNRLDefs::EnableType enable,
                bool report,
				MNRL::port_def inputDefs,
				MNRL::port_def outputDefs,
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
            port_map inputDefs;
            port_map outputDefs;
            std::map<std::string, std::shared_ptr<json11::Json>> attributes;

        private:
            std::shared_ptr<MNRL::port_map> validate_ports(MNRL::port_def &portdef);
    };


}

#endif
