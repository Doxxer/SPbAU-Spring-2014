CXX = clang++
CXXFLAGS = -Wall -Wextra -std=c++11 -pedantic
APPNAME = updatedb locate
OBJECTS = $(builddir)/FileSystemWalker.o $(builddir)/ThreadPool.o $(builddir)/DatabaseBuilder.o $(builddir)/utilities.o
BOOST_INCLUDE_PATHS = -I/usr/local/include/
BOOST_LIB_PATHS = -L/usr/local/lib/
LIBS = -lboost_system -lboost_thread-mt -lboost_filesystem -lboost_program_options -lboost_timer
srcdir = src
builddir = obj

all: $(builddir) $(APPNAME)

$(builddir):
	mkdir -p $(builddir)

####################

$(builddir)/locate.o: $(srcdir)/locate.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)

$(builddir)/updatedb.o: $(srcdir)/updatedb.cpp $(srcdir)/DatabaseBuilder.hpp $(srcdir)/FileSystemWalker.hpp $(srcdir)/ThreadPool.hpp $(srcdir)/utilities.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)
	
$(builddir)/FileSystemWalker.o: $(srcdir)/FileSystemWalker.cpp $(srcdir)/FileSystemWalker.hpp $(srcdir)/ThreadPool.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)
	
$(builddir)/ThreadPool.o: $(srcdir)/ThreadPool.cpp $(srcdir)/ThreadPool.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)
	
$(builddir)/DatabaseBuilder.o: $(srcdir)/DatabaseBuilder.cpp $(srcdir)/DatabaseBuilder.hpp $(srcdir)/FileSystemWalker.hpp $(srcdir)/ThreadPool.hpp $(srcdir)/SuffixIterator.hpp $(srcdir)/utilities.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)

$(builddir)/utilities.o: $(srcdir)/utilities.cpp $(srcdir)/utilities.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)

####################

updatedb: $(OBJECTS) $(builddir)/updatedb.o 
	$(CXX) $(CXXFLAGS) $(OBJECTS) $(builddir)/updatedb.o -o $@ $(BOOST_LIB_PATHS) $(LIBS)

locate: $(OBJECTS) $(builddir)/locate.o 
	$(CXX) $(CXXFLAGS) $(OBJECTS) $(builddir)/locate.o -o $@ $(BOOST_LIB_PATHS) $(LIBS)

clean:
	rm -rf $(builddir)
	rm -rf $(APPNAME)
	
run:
	# ./updatedb --database-root /Users/doxer/Documents --output "index.db"
	./locate --database "index.db" klm

.PHONY: clean