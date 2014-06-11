CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++11 -pedantic -O2 -Ofast
APPNAME = main
BOOST_INCLUDE_PATHS = -I/usr/local/include/
BOOST_LIB_PATHS = -L/usr/local/lib/
LIBS = -lboost_system -lboost_timer -ltbb
srcdir = src
builddir = obj

all: $(builddir) $(APPNAME)

$(builddir):
	mkdir -p $(builddir)

####################

$(builddir)/main.o: $(srcdir)/main.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)

####################

main: $(OBJECTS) $(builddir)/main.o 
	$(CXX) $(CXXFLAGS) $(builddir)/main.o -o $@ $(BOOST_LIB_PATHS) $(LIBS)

clean:
	rm -rf $(builddir)
	rm -rf $(APPNAME)
	
run:
	./main

.PHONY: clean