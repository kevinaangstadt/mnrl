/*
* Kevin Angstadt
* angstadt {at} umich.edu
*
* MNRLNetwork.cpp
*/

#include <cstdio>

#include <rapidjson/document.h>
#include <rapidjson/filewritestream.h>
#include <rapidjson/prettywriter.h>
#include <rapidjson/writer.h>


#include "MNRLNetwork.hpp"
#include "MNRLState.hpp"
#include "MNRLHState.hpp"
#include "MNRLUpCounter.hpp"
#include "MNRLBoolean.hpp"
#include "JSONWriter.hpp"

using namespace std;
using namespace MNRL;
using namespace rapidjson;

void MNRLNetwork::exportToFile(std::string filename, bool pretty) {
  Document d = JSONWriter::toJSON(*this);
  
  FILE *out = fopen(filename.c_str(), "wb");
  char writeBuffer[65536];
  
  FileWriteStream os(out, writeBuffer, sizeof(writeBuffer));
  
  if(pretty) {
    PrettyWriter<FileWriteStream> writer(os);
    d.Accept(writer);
  } else {
    Writer<FileWriteStream> writer(os);
    d.Accept(writer);
  }
  
  fclose(out);
}