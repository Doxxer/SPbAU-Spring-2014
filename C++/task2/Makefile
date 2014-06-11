CXX = clang++
CXXFLAGS = -Wall -Wextra -std=c++11 -pedantic -O2 -Ofast
APPNAME = updatedb locate
OBJECTS = $(builddir)/FileSystemWalker.o $(builddir)/ThreadPool.o $(builddir)/DatabaseBuilder.o $(builddir)/DatabaseScanner.o $(builddir)/Utilities.o
BOOST_INCLUDE_PATHS = -I/usr/local/include/
BOOST_LIB_PATHS = -L/usr/local/lib/
LIBS = -lboost_system -lboost_thread-mt -lboost_filesystem -lboost_program_options -lboost_timer -ltbb
srcdir = src
builddir = obj

all: $(builddir) $(APPNAME)

$(builddir):
	mkdir -p $(builddir)

####################

$(builddir)/Locate.o: $(srcdir)/Locate.cpp $(srcdir)/Utilities.hpp $(srcdir)/DatabaseScanner.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)

$(builddir)/UpdateDB.o: $(srcdir)/UpdateDB.cpp $(srcdir)/DatabaseBuilder.hpp $(srcdir)/FileSystemWalker.hpp $(srcdir)/ThreadPool.hpp $(srcdir)/Utilities.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)
	
$(builddir)/FileSystemWalker.o: $(srcdir)/FileSystemWalker.cpp $(srcdir)/FileSystemWalker.hpp $(srcdir)/ThreadPool.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)
	
$(builddir)/ThreadPool.o: $(srcdir)/ThreadPool.cpp $(srcdir)/ThreadPool.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)
	
$(builddir)/DatabaseBuilder.o: $(srcdir)/DatabaseBuilder.cpp $(srcdir)/DatabaseBuilder.hpp $(srcdir)/FileSystemWalker.hpp $(srcdir)/ThreadPool.hpp $(srcdir)/SuffixIterator.hpp $(srcdir)/Utilities.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)
	
$(builddir)/DatabaseScanner.o: $(srcdir)/DatabaseScanner.cpp $(srcdir)/Utilities.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)

$(builddir)/Utilities.o: $(srcdir)/Utilities.cpp $(srcdir)/Utilities.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@ $(BOOST_INCLUDE_PATHS)

####################

updatedb: $(OBJECTS) $(builddir)/UpdateDB.o 
	$(CXX) $(CXXFLAGS) $(OBJECTS) $(builddir)/UpdateDB.o -o $@ $(BOOST_LIB_PATHS) $(LIBS)

locate: $(OBJECTS) $(builddir)/Locate.o 
	$(CXX) $(CXXFLAGS) $(OBJECTS) $(builddir)/Locate.o -o $@ $(BOOST_LIB_PATHS) $(LIBS)

clean:
	rm -rf $(builddir)
	rm -rf $(APPNAME)
	
run:
	./updatedb --database-root /Users/doxer/Dropbox --output "index.db"
	./locate --database "index.db" xls

.PHONY: clean