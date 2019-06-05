/*
 * Kevin Angstadt
 * angstadt {at} virginia.edu
 *
 * MNRLNetwork.cpp
 */

 #include <json11.hpp>
 #include <json.hpp>

 #include "MNRLNetwork.hpp"
 #include "MNRLState.hpp"
 #include "MNRLHState.hpp"
 #include "MNRLUpCounter.hpp"
 #include "MNRLBoolean.hpp"
 #include "JSONWriter.hpp"

 using namespace std;
 using namespace MNRL;
 using namespace json11;
 
 void MNRLNetwork::exportToFile(std::string filename, bool pretty) {
   ofstream out(filename);
   if(pretty) {
    out << nlohmann::json::parse(JSONWriter::toJSON(*this).dump()).dump(4) << endl;
  } else {
    out << JSONWriter::toJSON(*this).dump() << endl;
  }
   out.close();
 }