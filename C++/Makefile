IDIR = ./include
SRCDIR = ./src
CC = g++-5
AR = ar
JSON11 = ./lib/json11/

CXXFLAGS = -I$(IDIR) -I./lib/valijson/include -I$(JSON11) -I./lib/valijson/thirdparty/rapidjson-1.0.2/include --std=c++11 -g -O0
ARFLAGS = rcs

_DEPS = mnrl.hpp MNRLNode.hpp MNRLDefs.hpp MNRLPort.hpp MNRLState.hpp
_OBJ_VALIDATE = validate.o 
_OBJ_MNRL = MNRLDefs.o MNRLPort.o MNRLNode.o MNRLState.o

ODIR = $(SRCDIR)/obj
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))
OBJ_VALIDATE = $(patsubst %,$(ODIR)/%,$(_OBJ_VALIDATE)) $(ODIR)/json11.o
OBJ_MNRL = $(patsubst %,$(ODIR)/%,$(_OBJ_MNRL)) $(ODIR)/json11.o

all: validate libmnrl

libmnrl: $(OBJ_MNRL)
	$(AR) $(ARFLAGS) $@.a $^ 

validate: $(OBJ_VALIDATE)
	$(CC) $(CXXFLAGS) -o $@ $^

$(ODIR)/%.o: $(SRCDIR)/%.cpp $(DEPS)
	@pwd
	@mkdir -p $(ODIR)
	$(CC) $(CXXFLAGS) -c -o $@ $<

$(ODIR)/json11.o: $(JSON11)/json11.cpp
	@mkdir -p $(ODIR)
	$(CC) $(CXXFLAGS) -c -o $@ $< 
	
.PHONY: clean

clean:
	rm -f $(ODIR)/*.o validate
	rmdir $(ODIR)