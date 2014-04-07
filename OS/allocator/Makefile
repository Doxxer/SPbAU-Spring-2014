CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++11 -pedantic
APPNAME = allocator
OBJECTS = $(builddir)/main.o $(builddir)/MemoryAllocator.o
srcdir = .
builddir = obj

all: $(builddir) $(APPNAME)

$(builddir):
	mkdir -p $(builddir)

####################

$(builddir)/main.o: $(srcdir)/main.cpp $(srcdir)/MemoryAllocator.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
	
$(builddir)/MemoryAllocator.o: $(srcdir)/MemoryAllocator.cpp $(srcdir)/MemoryAllocator.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

####################

$(APPNAME): $(OBJECTS)
	$(CXX) $(OBJECTS) -o $@

clean:
	rm -rf $(builddir)
	rm -rf $(APPNAME)
	
run:
	./$(APPNAME)

.PHONY: clean