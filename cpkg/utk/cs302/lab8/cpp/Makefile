OBJECTS= main.o graph.o
HEADERS= lab8.hpp
CFLAGS= -pedantic -Wall -O3

all:	mst

mst:	$(OBJECTS) $(HEADERS)
	g++ -o mst $(OBJECTS)

clean:
	rm -f *.o mst

.SUFFIXES: .cpp.o

.cpp.o: $(HEADERS)
	g++ $(CFLAGS) -c $*.cpp
