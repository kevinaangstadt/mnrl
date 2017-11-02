/*
 * Kevin Angstadt
 * angstadt {at} umich.edu
 *
 * MNRLHPDState.cpp
 */

#include "MNRLHPDState.hpp"

using namespace std;
using namespace MNRL;

static void validateEnable(MNRLDefs::EnableType en) {
    switch(en) {
	case MNRLDefs::EnableType::ENABLE_ON_ACTIVATE_IN:
	case MNRLDefs::EnableType::ENABLE_ON_START_AND_ACTIVATE_IN:
		return;
	default:
		// not supported
		throw MNRLError::InvalidEnableError(en); 
    }
}

//////////////////
// MNRLReportId //
//////////////////

// All stack actions on the same state
MNRLHPDState::MNRLHPDState(
    string symbolSet,
    string stackSet,
    bool popStack,
    string pushStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    shared_ptr<MNRLReportId> reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), symbolSet(symbolSet), stackSet(stackSet), popStack(popStack), pushStack(pushStack), reportId(reportId) {
    validateEnable(enable);
}

// No stack push
MNRLHPDState::MNRLHPDState(
    string symbolSet,
    string stackSet,
    bool popStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    shared_ptr<MNRLReportId> reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), symbolSet(symbolSet), stackSet(stackSet), popStack(popStack), reportId(reportId) {
    validateEnable(enable);
    compareInput = true;
    enablePush = false;
}

// Epsilon input matching
MNRLHPDState::MNRLHPDState(
    string stackSet,
    bool popStack,
    string pushStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    shared_ptr<MNRLReportId> reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), stackSet(stackSet), popStack(popStack), pushStack(pushStack), reportId(reportId) {
    validateEnable(enable);
    compareInput = false;
    enablePush = true;
}

// Epsilon input matching, no stack push
MNRLHPDState::MNRLHPDState(
    string stackSet,
    bool popStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    shared_ptr<MNRLReportId> reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), stackSet(stackSet), popStack(popStack), reportId(reportId) {
    validateEnable(enable);
    compareInput = false;
    enablePush = false;
}

///////////////////
// String Report //
///////////////////

// All stack actions on the same state
MNRLHPDState::MNRLHPDState(
    string symbolSet,
    string stackSet,
    bool popStack,
    string pushStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    string reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), symbolSet(symbolSet), stackSet(stackSet), popStack(popStack), pushStack(pushStack), reportId(shared_ptr<MNRLReportIdString>(new MNRLReportIdString(reportId))) {
    validateEnable(enable);
    compareInput = true;
    enablePush = true;
}

// No stack push
MNRLHPDState::MNRLHPDState(
    string symbolSet,
    string stackSet,
    bool popStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    string reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), symbolSet(symbolSet), stackSet(stackSet), popStack(popStack), reportId(shared_ptr<MNRLReportIdString>(new MNRLReportIdString(reportId))) {
    validateEnable(enable);
    compareInput = true;
    enablePush = false;
}

// Epsilon input matching
MNRLHPDState::MNRLHPDState(
    string stackSet,
    bool popStack,
    string pushStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    string reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), stackSet(stackSet), popStack(popStack), pushStack(pushStack), reportId(shared_ptr<MNRLReportIdString>(new MNRLReportIdString(reportId))) {
    validateEnable(enable);
    compareInput = false;
    enablePush = true;
}

// Epsilon input matching, no stack push
MNRLHPDState::MNRLHPDState(
    string stackSet,
    bool popStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    string reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), stackSet(stackSet), popStack(popStack), reportId(shared_ptr<MNRLReportIdString>(new MNRLReportIdString(reportId))) {
    validateEnable(enable);
    compareInput = false;
    enablePush = false;
}

////////////////////
// Integer Report //
////////////////////

// All stack actions on the same state
MNRLHPDState::MNRLHPDState(
    string symbolSet,
    string stackSet,
    bool popStack,
    string pushStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    int reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), symbolSet(symbolSet), stackSet(stackSet), popStack(popStack), pushStack(pushStack), reportId(shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(reportId))) {
    validateEnable(enable);
    compareInput = true;
    enablePush = true;
}

// No stack push
MNRLHPDState::MNRLHPDState(
    string symbolSet,
    string stackSet,
    bool popStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    int reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), symbolSet(symbolSet), stackSet(stackSet), popStack(popStack), reportId(shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(reportId))) {
    validateEnable(enable);
    compareInput = true;
    enablePush = false;
}

// Epsilon input matching
MNRLHPDState::MNRLHPDState(
    string stackSet,
    bool popStack,
    string pushStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    int reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), stackSet(stackSet), popStack(popStack), pushStack(pushStack), reportId(shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(reportId))) {
    validateEnable(enable);
    compareInput = false;
    enablePush = true;
}

// Epsilon input matching, no stack push
MNRLHPDState::MNRLHPDState(
    string stackSet,
    bool popStack,
    MNRLDefs::EnableType enable,
    string id,
    bool report,
    int reportId,
    shared_ptr<map<string,string>> attributes
) : MNRLNode(
    id,
    enable,
    report,
    gen_input(),
    gen_output(),
    attributes
), stackSet(stackSet), popStack(popStack), reportId(shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(reportId))) {
    validateEnable(enable);
    compareInput = false;
    enablePush = false;
}

MNRLHPDState::~MNRLHPDState() {}

shared_ptr<MNRLReportId> MNRLHPDState::getReportId() { return reportId; }
void MNRLHPDState::setReportId(string id) { reportId = shared_ptr<MNRLReportIdString>(new MNRLReportIdString(id)); }
void MNRLHPDState::setReportId(int id) { reportId = shared_ptr<MNRLReportIdInt>(new MNRLReportIdInt(id)); }
void MNRLHPDState::setReportId(shared_ptr<MNRLReportId> id) { reportId = id; }

string MNRLHPDState::getSymbolSet() { return symbolSet; }
void MNRLHPDState::setSymbolSet(string set) { symbolSet = set; compareInput = true; }
void MNRLHPDState::removeSymbolSet() { compareInput = false; }

string MNRLHPDState::getStackSet() { return stackSet; }
void MNRLHPDState::setStackSet(string set) { stackSet = set; }

string MNRLHPDState::getPushSymbol() { return pushStack; }
void MNRLHPDState::setPushSymbol(string sym) { pushStack = sym; enablePush = true; }
void MNRLHPDState::removePushSymbol() { enablePush = false; }

bool MNRLHPDState::getPop() { return popStack; }
void MNRLHPDState::setPop(bool p) { popStack = p; }

bool MNRLHPDState::doesStackPush() { return enablePush; }
bool MNRLHPDState::isEpsilonInput() { return !compareInput; }