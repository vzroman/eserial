
CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

C_SRC_DIR = $(CURDIR)
INSTALL_DIR = $(BASEDIR)/priv
C_SRC_OUTPUT ?= $(INSTALL_DIR)/$(PROJECT)

CC = g++
CFLAGS = -O3 -finline-functions -Wall -Wmissing-prototypes
CXXFLAGS = -O3 -finline-functions -Wall

CFLAGS += -fPIC
CXXFLAGS += -fPIC

COMPILE_C = $(CC) $(CFLAGS) $(CPPFLAGS)
COMPILE_CPP = $(CC) $(CXXFLAGS) $(CPPFLAGS)

build: $(C_SRC_OUTPUT)
	@chmod +x $(C_SRC_OUTPUT)

$(C_SRC_OUTPUT): eserial.o
	@mkdir -p $(INSTALL_DIR)
	$(COMPILE_CPP) -o $@ $<

eserial.o: eserial.cpp
	$(COMPILE_CPP) -c $<

clean:
	@rm -rf $(C_SRC_OUTPUT) $(C_SRC_DIR)/eserial.o
