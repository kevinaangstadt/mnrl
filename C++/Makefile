IDIR = ./include
SRCDIR = ./src

CC = g++-5
AR = ar
LD = ld
OBJCPY = gobjcopy

JSON11 = ./lib/json11


NAME = libmnrl
SNAME = $(NAME).a
DNAME = $(NAME).so

CXXFLAGS = -I$(IDIR) -I./lib/valijson/include -I$(JSON11) --std=c++11 -fPIC
LDFLAGS = -L.
ARFLAGS = rcs

_DEPS = *.hpp
_OBJ_VALIDATE = validate.o 
_OBJ_MNRL = mnrl.o mnrl_schema.o MNRLDefs.o MNRLPort.o MNRLNode.o MNRLState.o MNRLHState.o MNRLUpCounter.o MNRLBoolean.o MNRLNetwork.o

ODIR = $(SRCDIR)/obj
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))
OBJ_VALIDATE = $(patsubst %,$(ODIR)/%,$(_OBJ_VALIDATE)) $(ODIR)/json11.o
OBJ_MNRL = $(patsubst %,$(ODIR)/%,$(_OBJ_MNRL)) $(ODIR)/json11.o

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	NASMFLAGS = -felf64
endif
ifeq ($(UNAME_S),Darwin)
	NASMFLAGS = -fmacho64
endif

all: validate libmnrl

libmnrl: $(SNAME) $(DNAME)

$(DNAME): LDFLAGS += -shared
$(DNAME): $(OBJ_MNRL)
	$(CC) $(LDFLAGS) $^ -o $@

$(SNAME): $(OBJ_MNRL)
	$(AR) $(ARFLAGS) $@ $^ 

validate: $(OBJ_VALIDATE)
	$(CC) $(CXXFLAGS) -o $@ $^

$(ODIR)/%.o: $(SRCDIR)/%.cpp $(DEPS)
	@pwd
	@mkdir -p $(ODIR)
	$(CC) $(CXXFLAGS) -c -o $@ $<

$(ODIR)/json11.o: $(JSON11)/json11.cpp
	@mkdir -p $(ODIR)
	$(CC) $(CXXFLAGS) -c -o $@ $< 

$(ODIR)/%.o: $(SRCDIR)/%.asm
	nasm $(NASMFLAGS) -o $@ $<
	
	#$(OBJCPY) -B i386 -I binary -O mach-o-x86-64 --rename-section .data=.const mnrl-schema.json $(ODIR)/mnrl_schema.o
	
.PHONY: clean

clean:
	rm -f $(ODIR)/*.o validate $(SNAME) $(DNAME)
	rmdir $(ODIR)