CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -pedantic
srcdir = .
builddir = .

all: $(builddir)/init.o
	$(CXX) $(builddir)/init.o -o $(builddir)/init

$(builddir)/init.o: $(srcdir)/init.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f *.o $(APPNAME)
	
run:
	$(builddir)/init /Users/doxer/Documents/GitRepositories/SPbAU-Spring-2014/OS/filesystem/fs

.PHONY: clean