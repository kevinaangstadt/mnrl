IDIR = ./include
SRCDIR = ./src

CC = g++
AR = ar
LD = ld

RAPIDJSON = ./lib/rapidjson/include
RAPIDJSON_LIC = $(RAPIDJSON)/../license.txt

NAME = libmnrl
SNAME = $(NAME).a
DNAME = $(NAME).so

CXXFLAGS = -Wconversion -Wall -Werror -Wextra -pedantic 
CXXFLAGS += -I$(IDIR) -I $(RAPIDJSON) --std=c++11 -fPIC
LDFLAGS = -L.
ARFLAGS = rcs

_DEPS = *.hpp
_OBJ_MNRL = mnrl.o mnrl_schema.o MNRLNetwork.o MNRLDefs.o

ODIR = $(SRCDIR)/obj
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))
OBJ_VALIDATE = $(patsubst %,$(ODIR)/%,$(_OBJ_VALIDATE))
OBJ_MNRL = $(patsubst %,$(ODIR)/%,$(_OBJ_MNRL))

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	NASMFLAGS = -felf64
endif
ifeq ($(UNAME_S),Darwin)
	NASMFLAGS = -fmacho64
endif
ifeq ($(OS),Windows_NT)
	NASMFLAGS = -fwin64
endif

###########
# Testing
TESTDIR = tests
TESTLDFLAGS = -lmnrl
###########

all: release
	
release: CXXFLAGS += -O3
release: libmnrl

debug: CXXFLAGS += -g3
debug: libmnrl

libmnrl: $(RAPIDJSON_LIC) $(SNAME) $(DNAME)

$(DNAME): LDFLAGS += -shared
$(DNAME): $(OBJ_MNRL)
	$(CC) $(LDFLAGS) $^ -o $@

$(SNAME): $(OBJ_MNRL)
	$(AR) $(ARFLAGS) $@ $^ 

$(ODIR)/%.o: $(SRCDIR)/%.cpp $(DEPS)
	@pwd
	@mkdir -p $(ODIR)
	$(CC) $(CXXFLAGS) -c -o $@ $<

$(ODIR)/%.o: $(SRCDIR)/%.asm
	nasm $(NASMFLAGS) -o $@ $<

$(RAPIDJSON_LIC):
	git submodule init
	git submodule update	

test: tests 
	../tests/readwrite_test.sh tests/readwrite tests/

tests: libmnrl $(TESTDIR)/readwrite $(TESTDIR)/readandprint

$(TESTDIR)/readwrite: $(TESTDIR)/readwrite.cpp
	$(CC) $(CXXFLAGS) $(LDFLAGS) $< $(TESTLDFLAGS) -o $@ 

$(TESTDIR)/readandprint: $(TESTDIR)/readandprint.cpp
	$(CC) -g3 $(CXXFLAGS) $(LDFLAGS) $< $(TESTLDFLAGS) -o $@

.PHONY: all release debug test clean

clean:
	rm -f $(ODIR)/*.o $(SNAME) $(DNAME) tests/readwrite tests/readandprint
	rmdir $(ODIR)
