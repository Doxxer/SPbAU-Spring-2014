CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -pedantic
APPNAME = scheduler
srcdir = .
builddir = .

all: $(builddir)/main.o $(builddir)/process.hpp $(builddir)/scheduler.o $(builddir)/process.o
	$(CXX) $(builddir)/main.o $(builddir)/scheduler.o $(builddir)/process.o -o $(builddir)/$(APPNAME)

$(builddir)/main.o: $(srcdir)/main.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(builddir)/scheduler.o: $(srcdir)/scheduler.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(builddir)/process.o: $(srcdir)/process.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f *.o $(APPNAME)
	
run:
	$(builddir)/$(APPNAME)

.PHONY: clean