// Kevin Angstadt
// angstadt {at} umich.edu
//
// MNRLHPDState Object

#ifndef MNRLHPDSTATE_HPP
#define MNRLHPDSTATE_HPP

#include <string>
#include <utility>
#include <vector>
#include <map>

#include "MNRLDefs.hpp"
#include "MNRLNode.hpp"
#include "MNRLPort.hpp"
#include "MNRLReportId.hpp"

namespace MNRL{
    class MNRLDefs;
    class MNRLNode;
    class MNRLPort;
    class MNRLHPDState : public MNRLNode {
        public:
            
            //////////////////
            // MNRLReportId //
            //////////////////
            
            // All stack actions on the same state
            MNRLHPDState(
                std::string symbolSet,
                std::string stackSet,
                bool popStack,
                std::string pushStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                std::shared_ptr<MNRLReportId> reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            // No stack push
            MNRLHPDState(
                std::string symbolSet,
                std::string stackSet,
                bool popStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                std::shared_ptr<MNRLReportId> reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            // Epsilon input matching
            MNRLHPDState(
                std::string stackSet,
                bool popStack,
                std::string pushStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                std::shared_ptr<MNRLReportId> reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            // Epsilon input matching, no stack push
            MNRLHPDState(
                std::string stackSet,
                bool popStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                std::shared_ptr<MNRLReportId> reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            ///////////////////
            // String Report //
            ///////////////////
            
            // All stack actions on the same state
            MNRLHPDState(
                std::string symbolSet,
                std::string stackSet,
                bool popStack,
                std::string pushStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                std::string reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            // No stack push
            MNRLHPDState(
                std::string symbolSet,
                std::string stackSet,
                bool popStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                std::string reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            // Epsilon input matching
            MNRLHPDState(
                std::string stackSet,
                bool popStack,
                std::string pushStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                std::string reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            // Epsilon input matching, no stack push
            MNRLHPDState(
                std::string stackSet,
                bool popStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                std::string reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            ////////////////////
            // Integer Report //
            ////////////////////
            
            // All stack actions on the same state
            MNRLHPDState(
                std::string symbolSet,
                std::string stackSet,
                bool popStack,
                std::string pushStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                int reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            // No stack push
            MNRLHPDState(
                std::string symbolSet,
                std::string stackSet,
                bool popStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                int reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            // Epsilon input matching
            MNRLHPDState(
                std::string stackSet,
                bool popStack,
                std::string pushStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                int reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            // Epsilon input matching, no stack push
            MNRLHPDState(
                std::string stackSet,
                bool popStack,
                MNRLDefs::EnableType enable,
                std::string id,
                bool report,
                int reportId,
                std::shared_ptr<std::map<std::string,std::string>> attributes
            );
            
            virtual ~MNRLHPDState();
            
            virtual MNRLDefs::NodeType getNodeType() { return MNRLDefs::NodeType::HPDSTATE; }
            
            std::shared_ptr<MNRLReportId> getReportId();
            void setReportId(std::string id);
			void setReportId(int id);
			void setReportId(std::shared_ptr<MNRLReportId> id);
            
            std::string getSymbolSet();
            void setSymbolSet(std::string set);
            void removeSymbolSet();
            
            std::string getStackSet();
            void setStackSet(std::string set);
            
            std::string getPushSymbol();
            void setPushSymbol(std::string sym);
            void removePushSymbol();
            
            bool getPop();
            void setPop(bool p);
            
            bool doesStackPush();
            bool isEpsilonInput();
            
        protected:
            std::string symbolSet = "";
            std::string stackSet;
            std::string pushStack = 0;
            bool popStack;
            
            bool compareInput;
            bool enablePush;
            
            std::shared_ptr<MNRLReportId> reportId;
        
        private:
			static port_def gen_input() {
				port_def in;
				in.push_back(
					std::shared_ptr<MNRLPort>( new MNRLPort(
							MNRLDefs::H_PD_STATE_INPUT,
							1
						)
					)
				);
				return in;
			}
			static port_def gen_output() {
				port_def outs;
				outs.push_back(
					std::shared_ptr<MNRLPort>( new MNRLPort(
							MNRLDefs::H_PD_STATE_OUTPUT,
							1
						)
					)
				);
				return outs;
			}
    };
}

#endif 