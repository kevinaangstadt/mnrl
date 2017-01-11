// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLNetwork Object

#ifndef MNRLNETWORK_HPP
#define MNRLNETWORK_HPP

#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <json11.hpp>
#include <json.hpp>
#include <fstream>

#include "MNRLDefs.hpp"
#include "MNRLError.hpp"

namespace MNRL {
	class MNRLNode;
	class MNRLState;
	class MNRLHState;
	class MNRLUpCounter;
	class MNRLBoolean;
    class MNRLNetwork {
        public:
            MNRLNetwork(std::string id);
            virtual ~MNRLNetwork();
            virtual json11::Json toJSON();
            virtual void exportToFile(std::string filename);
            std::shared_ptr<MNRLNode> getNodeById(std::string id);
            std::shared_ptr<MNRLNode> addNode(std::shared_ptr<MNRLNode> theNode);
            
            std::shared_ptr<MNRLState> addState(
            	std::shared_ptr<std::vector<std::pair<std::string,std::string>>> outputSymbols,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                int reportId,
                bool latched,
                std::shared_ptr<json11::Json::object> attributes
            );
            
            std::shared_ptr<MNRLState> addState(
				std::shared_ptr<std::vector<std::pair<std::string,std::string>>> outputSymbols,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				int reportId,
				bool latched
			);

            std::shared_ptr<MNRLHState> addHState(
                std::string symbols,
				MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                int reportId,
                bool latched,
				std::shared_ptr<json11::Json::object> attributes
            );
            
            std::shared_ptr<MNRLHState> addHState(
				std::string symbols,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				int reportId,
				bool latched
			);

            std::shared_ptr<MNRLUpCounter> addUpCounter(
                int threshold,
                MNRLDefs::CounterMode mode,
                std::string id,
                bool report,
                int reportId,
				std::shared_ptr<json11::Json::object> attributes
            );
            
            std::shared_ptr<MNRLUpCounter> addUpCounter(
				int threshold,
				MNRLDefs::CounterMode mode,
				std::string id,
				bool report,
				int reportId
			);

            std::shared_ptr<MNRLBoolean> addBoolean(
                MNRLDefs::BooleanMode booleanType,
				MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                int reportId,
				std::shared_ptr<json11::Json::object> attributes
            );
            
            std::shared_ptr<MNRLBoolean> addBoolean(
				MNRLDefs::BooleanMode booleanType,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report,
				int reportId
			);

            void addConnection(std::string src, std::string src_port, std::string dest, std::string dest_port);
            void removeConnection(std::string src, std::string src_port, std::string dest, std::string dest_port);
        
        protected:
            std::string id;
            std::map <std::string,std::shared_ptr<MNRLNode>> nodes;
            unsigned long nodes_added;
            
            std::string getUniqueNodeId(std::string id);
    };
}
#endif
