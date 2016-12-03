IDIR = ./include
SRCDIR = ./src
CC = g++-5
JSON11 = ./lib/json11/

CXXFLAGS = -I$(IDIR) -I./lib/valijson/include -I$(JSON11) -I./lib/valijson/thirdparty/rapidjson-1.0.2/include --std=c++11 -g -O0

_DEPS = *.hpp
_OBJ = validate.o

ODIR = $(SRCDIR)/obj
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

all: validate

validate: $(OBJ) $(ODIR)/json11.o
	$(CC) $(CXXFLAGS) -o $@ $^

$(ODIR)/%.o: $(SRCDIR)/%.cpp $(DEPS)
	@pwd
	@mkdir -p $(ODIR)
	$(CC) $(CXXFLAGS) -c -o $@ $<

$(ODIR)/json11.o: $(JSON11)/json11.cpp
	@mkdir -p $(ODIR)
	$(CC) $(CXXFLAGS) -c -o $@ $< $(CXXFLAGS)
	
.PHONY: clean

clean:
	rm -f $(ODIR)/*.o validate
	rmdir $(ODIR)