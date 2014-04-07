CXX = g++
CXXFLAGS = -O2 -Wall -Wextra -std=c++11 -pedantic
APPNAME = bot
OBJECTS = $(builddir)/main.o $(builddir)/game.o
srcdir = .
builddir = obj

all: $(builddir) $(APPNAME)

$(builddir):
	mkdir -p $(builddir)

####################

$(builddir)/main.o: $(srcdir)/main.cpp $(srcdir)/Game.hpp $(srcdir)/utils.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
	
$(builddir)/game.o: $(srcdir)/Game.cpp $(srcdir)/utils.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

####################

$(APPNAME): $(OBJECTS)
	$(CXX) $(CXXFLAGS) $(OBJECTS) -o $@

clean:
	rm -rf $(builddir)
	rm -rf $(APPNAME)
	
run:
	test/runner.py -p ./bot -p test/rnd.py -g 100

.PHONY: clean