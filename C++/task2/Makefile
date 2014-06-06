CXX = clang++
CXXFLAGS = -Wall -Wextra -std=c++11 -pedantic
APPNAME = updatedb
OBJECTS = $(builddir)/main.o $(builddir)/FileSystemWalker.o $(builddir)/ThreadPool.o
BOOST_INCLUDE_PATHS = -I/usr/local/Cellar/boost/1.55.0_1/include/
BOOST_LIB_PATHS = -L/usr/local/Cellar/boost/1.55.0_1/lib/
LIBS = -lboost_system -lboost_thread-mt -lboost_filesystem -lboost_program_options -lboost_timer
srcdir = .
builddir = obj

all: clean $(builddir) $(APPNAME)

$(builddir):
	mkdir -p $(builddir)

####################

$(builddir)/main.o: $(srcdir)/main.cpp $(srcdir)/FileSystemWalker.hpp $(srcdir)/ThreadPool.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)
	
$(builddir)/FileSystemWalker.o: $(srcdir)/FileSystemWalker.cpp $(srcdir)/FileSystemWalker.hpp $(srcdir)/ThreadPool.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)
	
$(builddir)/ThreadPool.o: $(srcdir)/ThreadPool.cpp $(srcdir)/ThreadPool.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)
	
####################

$(APPNAME): $(OBJECTS)
	$(CXX) $(CXXFLAGS) $(OBJECTS) -o $@ $(BOOST_LIB_PATHS) $(LIBS)

clean:
	rm -rf $(builddir)
	rm -rf $(APPNAME)
	
run:
	./$(APPNAME) --database-root /Users/doxer/Documents --output "path to file"

.PHONY: clean