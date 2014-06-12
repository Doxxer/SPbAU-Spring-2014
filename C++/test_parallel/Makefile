CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++11 -pedantic -O2 -Ofast
APPNAME = main
LIBS = -lboost_system -lboost_timer -ltbb
srcdir = src
builddir = obj

UNAME := $(shell uname -s)

ifeq ($(UNAME), Darwin)
    INCLUDE_PATHS = -I/usr/local/include/
    LIB_PATHS = -L/usr/local/lib/
endif

all: $(builddir) $(APPNAME)

$(builddir):
	mkdir -p $(builddir)

####################

$(builddir)/main.o: $(srcdir)/main.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(INCLUDE_PATHS)

####################

main: $(OBJECTS) $(builddir)/main.o 
	$(CXX) $(CXXFLAGS) $(builddir)/main.o -o $@ $(LIB_PATHS) $(LIBS)

clean:
	rm -rf $(builddir)
	rm -rf $(APPNAME)
	
run:
	./main

.PHONY: clean