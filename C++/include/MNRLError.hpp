// Kevin Angstadt
// angstadt {at} virginia.edu
//
// MNRLError.hpp

#ifndef MNRLERROR_HPP
#define MNRLERROR_HPP

#include <stdexcept>

namespace MNRL{
	namespace MNRLError {
		class MNRLError : public std::runtime_error {
		public:
			MNRLError() : runtime_error("Unspecified MNRL Error!") {}
			MNRLError(std::string msg):runtime_error(msg.c_str()){}
		};

		class DuplicateIdError: public MNRLError {
		public:
			DuplicateIdError(std::string id) : MNRLError(id + " is already defined"), id(id) {}
			std::string get_id() { return id; }
		private:
			std::string id;
		};

		class DuplicatePortError: public MNRLError {
		public:
			DuplicatePortError(std::string port_id) : MNRLError(port_id + " is already defined"), port_id(port_id) {}
			std::string get_port_id() { return port_id; }
		private:
			std::string port_id;
		};

		class EnableError : public MNRLError {
		public:
			EnableError(std::string enable) : MNRLError("unknown enable code " + enable), enable(enable) {}
			std::string get_enable() { return enable; }
		private:
			std::string enable;
		};

		class UpCounterThresholdError : public MNRLError {
		public:
			UpCounterThresholdError(int threshold) : MNRLError("threshold must be a non-negative integer"), threshold(threshold) {}
			int get_threshold() { return threshold; }
		private:
			int threshold;
		};

		class UpCounterModeError : public MNRLError {
		public:
			UpCounterModeError(std::string mode) : MNRLError("invalid mode " + mode), mode(mode) {}
			std::string get_mode() { return mode; }
		private:
			std::string mode;
		};

		class InvalidGatePortCount : public MNRLError {
		public:
			InvalidGatePortCount(int port_count) : MNRLError("gate port count must be a positive integer"), port_count(port_count) {}
			int get_port_count() { return port_count; }
		private:
			int port_count;
		};

		class InvalidGateType : public MNRLError {
		public:
			InvalidGateType(std::string gate) : MNRLError("unknown gate type " + gate), gate(gate) {}
			std::string get_gate() { return gate; }
		private:
			std::string gate;
		};

		class UnknownNode : public MNRLError{
		public:
			UnknownNode(std::string id) : MNRLError("node not found"), id(id) {}
			std::string get_id() { return id; }
		private:
			std::string id;
		};

		class UnknownPort : public MNRLError {
		public:
			UnknownPort(std::string id) : MNRLError("port not found: " + id), id(id) {}
			std::string get_id() { return id; }
		private:
			std::string id;
		};

		class PortWidthMismatch : public MNRLError {
		public:
			PortWidthMismatch(int source, int destination) : MNRLError("port widths do not align"), source(source), destination(destination) {}
			int get_source() { return source; }
			int get_destination() { return destination; }
		private:
			int source, destination;
		};
	}
}

#endif
