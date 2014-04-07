CXX = g++
CXXFLAGS = -O2 -Wall -Wextra -std=c++11 -pedantic -Ofast -ftree-vectorize -DDEPTH=5
APPNAME = bot
OBJECTS = $(builddir)/main.o $(builddir)/game.o
srcdir = .
builddir = obj

all: clean $(builddir) $(APPNAME)

$(builddir):
	mkdir -p $(builddir)

####################

$(builddir)/main.o: $(srcdir)/main.cpp $(srcdir)/Game.hpp $(srcdir)/Player.hpp $(srcdir)/SegmentsGenerator.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
	
$(builddir)/game.o: $(srcdir)/Game.cpp $(srcdir)/Game.hpp $(srcdir)/Player.hpp $(srcdir)/SegmentsGenerator.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

####################

$(APPNAME): $(OBJECTS)
	$(CXX) $(CXXFLAGS) $(OBJECTS) -o $@

clean:
	rm -rf $(builddir)
	rm -rf $(APPNAME)
	
run:
	test/runner.py -p ./bot -p test/rnd.py -g 10

.PHONY: clean