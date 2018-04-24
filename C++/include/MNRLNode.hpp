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
#include "MNRLDefs.hpp"
#include "MNRLError.hpp"
#include "MNRLPort.hpp"


namespace MNRL {
	typedef std::vector<std::shared_ptr<MNRLPort>> port_def;

    class MNRLNode {
        public:
			MNRLNode(
                std::string id,
                MNRLDefs::EnableType enable,
                bool report,
								port_def inputDefs,
								port_def outputDefs,
                std::shared_ptr<std::map<std::string,std::string>> attributes,
								MNRLDefs::ReportEnableType report_enable = MNRLDefs::ReportEnableType::ENABLE_ALWAYS
            );
            virtual ~MNRLNode();

            virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::NODE; }

            std::shared_ptr<port_map> getOutputConnections();
            std::shared_ptr<port_map> getInputConnections();

            std::shared_ptr<MNRLPort> getOutputPort(std::string portId);
            std::shared_ptr<MNRLPort> getInputPort(std::string portId);

            std::string getId();
            bool getReport();
            MNRLDefs::EnableType getEnable();
						MNRLDefs::ReportEnableType getReportEnable();
            std::shared_ptr<std::map<std::string,std::string>> getAttributes();

            void setId(std::string new_id);
            void setReport(bool r);
            void setEnable(MNRLDefs::EnableType e);
						void setReportEnable(MNRLDefs::ReportEnableType r);

        protected:
            std::string id;
            bool report;
						MNRLDefs::ReportEnableType reportEnable;
            MNRLDefs::EnableType enable;
            std::shared_ptr<port_map> inputDefs;
            std::shared_ptr<port_map> outputDefs;
            std::shared_ptr<std::map<std::string,std::string>> attributes;

        private:
            static std::shared_ptr<port_map> validate_ports(port_def &portdef) {
            	std::shared_ptr<port_map> ports = std::shared_ptr<port_map>(new port_map());

				for (std::shared_ptr<MNRLPort> p : portdef) {
					ports->insert(port_map::value_type(p->getId(),p));
				}

				return ports;
            }
    };


}

#endif
