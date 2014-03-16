CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -pedantic
APPNAME = main
srcdir = .
builddir = .

all: $(builddir)/main.o
	$(CXX) $(builddir)/main.o -o $(builddir)/$(APPNAME)

$(builddir)/main.o: $(srcdir)/task2.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
    
clean:
	rm -f *.o $(APPNAME)
	
run:
	$(builddir)/$(APPNAME)

.PHONY: clean