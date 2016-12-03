// Kevin Angstadt
// angstadt {at} virginia.edu
// University of Virginia
//
// C++ objects for manipulating MNRL files

#include <unordered_map>
#include <string>

namespace MNRL {
    class MNRLNetwork {
        public:
            MNRLNetwork(string id);
            ~MNRLNetwork();
            std::string toJSON();
            MNRLNode getNodeById(string id);
            MNRLNode addNode(MNRLNode theNode);
            
            State addState(
                FIXME outputSymbols,
                FIXME enable,
                string id,
                bool report,
                string reportId,
                bool latched,
                FIXME attributes
            );
            
            HState addHState(
                string symbols,
                FIXME enable,
                string id,
                bool report,
                string reportId,
                bool latched,
                FIXME attributes
            );
            
            UpCounter addUpCounter(
                unsigned long threshold,
                FIXME mode,
                string id,
                bool report,
                string reportId,
                FIXME attributes
            );
            
            Boolean addBoolean(
                FIXME booleanType,
                FIXME enable,
                string id,
                bool report,
                string reportId,
                FIXME attributes
            );
            
            void addConnection(string src, string dest);
            void addConnection(string src, string src_port, string dest, string dest_port);
            
            void removeConnection(string src, string dest);
            void removeConnection(string src, string src_port, string dest, string dest_port);
        
        protected:
            string id;
            unordered_map <string,MNRLNode> nodes;
            unsigned long nodes_added;
            
            string getUniqueNodeId(string id);
    }
}