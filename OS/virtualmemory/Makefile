CXX = clang++
CXXFLAGS = -Wall -Wextra -std=c++11 -pedantic
APPNAME = memory
OBJECTS = $(builddir)/main.o
srcdir = .
builddir = obj

all: $(builddir) $(APPNAME)

$(builddir):
	mkdir -p $(builddir)

########################################

$(builddir)/main.o: $(srcdir)/main.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
	
########################################

$(APPNAME): $(OBJECTS)
	$(CXX) $(OBJECTS) -o $@

clean:
	rm -rf $(builddir)
	rm -rf $(APPNAME)

run: 
	./$(APPNAME)
	
test: 
	./$(APPNAME) < test1.txt
	./$(APPNAME) < test2.txt

.PHONY: clean