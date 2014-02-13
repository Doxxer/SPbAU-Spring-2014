CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++03 -pedantic
APPNAME = shell
srcdir = .
builddir = .

all: $(builddir)/main.o $(builddir)/command.o
	$(CXX) $(builddir)/main.o $(builddir)/command.o -o $(builddir)/$(APPNAME)

$(builddir)/main.o: $(srcdir)/main.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
	
$(builddir)/command.o: $(srcdir)/command.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
    
clean:
	rm -f *.o $(APPNAME)
	
run:
	$(builddir)/$(APPNAME)

.PHONY: clean