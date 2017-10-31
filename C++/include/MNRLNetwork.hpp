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
#include <fstream>

#include "MNRLDefs.hpp"
#include "MNRLError.hpp"
#include "MapIterator.hpp"

namespace MNRL {
	class MNRLNode;
	class MNRLState;
	class MNRLHState;
	class MNRLUpCounter;
	class MNRLBoolean;
	class MNRLHPDState;
    class MNRLNetwork {
        public:
            MNRLNetwork(std::string id);
            virtual ~MNRLNetwork();
            virtual void exportToFile(std::string filename);

            MapIterator<std::map<std::string,std::shared_ptr<MNRLNode>>> getNodeIterator();
            std::map<std::string,std::shared_ptr<MNRLNode>> getNodes();

            std::shared_ptr<MNRLNode> getNodeById(std::string id);
            std::shared_ptr<MNRLNode> addNode(std::shared_ptr<MNRLNode> theNode);
            
            std::shared_ptr<MNRLState> addState(
            	std::shared_ptr<std::vector<std::pair<std::string,std::string>>> outputSymbols,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                int reportId,
                bool latched,
                std::shared_ptr<std::map<std::string,std::string>> attributes
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
				std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            std::shared_ptr<MNRLHState> addHState(
            		std::string symbols,
					MNRLDefs::EnableType enable,
					std::string id,
					bool report,
					std::string reportId,
					bool latched,
					std::shared_ptr<std::map<std::string,std::string>> attributes
            );

            std::shared_ptr<MNRLHState> addHState(
            		std::string symbols,
					MNRLDefs::EnableType enable,
					std::string id,
					bool report,
					bool latched,
					std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            std::shared_ptr<MNRLHState> addHState(
				std::string symbols,
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
					std::string reportId,
					bool latched
            );

            std::shared_ptr<MNRLHState> addHState(
            		std::string symbols,
					MNRLDefs::EnableType enable,
					std::string id,
					bool report,
					bool latched
            );
			
			/*
			 * H PD States
			 */
			
			// all stack actions
			std::shared_ptr<MNRLHPDState> addHPDState(
				std::string symbolSet,
				std::string stackSet,
				bool popStack,
				char pushStack,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report
			);
			// no stack push
			std::shared_ptr<MNRLHPDState> addHPDState(
				std::string symbolSet,
				std::string stackSet,
				bool popStack,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report
			);
			// input epsilon
			std::shared_ptr<MNRLHPDState> addHPDState(
				std::string stackSet,
				bool popStack,
				char pushStack,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report
			);
			// epsilon, no stack push
			std::shared_ptr<MNRLHPDState> addHPDState(
				std::string stackSet,
				bool popStack,
				MNRLDefs::EnableType enable,
				std::string id,
				bool report
			);

            std::shared_ptr<MNRLUpCounter> addUpCounter(
                int threshold,
                MNRLDefs::CounterMode mode,
                std::string id,
                bool report,
                int reportId,
				std::shared_ptr<std::map<std::string,std::string>> attributes
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
				std::shared_ptr<std::map<std::string,std::string>> attributes
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

            std::string getId();
        
        protected:
            std::string id;
            std::map <std::string,std::shared_ptr<MNRLNode>> nodes;
            unsigned long nodes_added;
            
            std::string getUniqueNodeId(std::string id);
    };
}
#endif
