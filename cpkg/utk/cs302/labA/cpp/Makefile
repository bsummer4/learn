OBJECTS= main.o dice.o graph.o
HEADERS= main.hpp dice.hpp graph.hpp
CFLAGS= -O3 -pedantic -Wall

all: worddice Makefile

worddice: $(OBJECTS)
	g++ $(CFLAGS) -o worddice $(OBJECTS)

clean:
	rm -f worddice $(OBJECTS)

.SUFFIXES:	.cpp.p

.cpp.o:	$(HEADERS)
	g++ $(CFLAGS) -c $*.cpp