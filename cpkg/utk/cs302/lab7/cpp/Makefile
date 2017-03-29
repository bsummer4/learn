OBJECTS= io.o main.o maze.o
HEADERS= lab7.h
CFLAGS= -pedantic -Wall -O3

all:	maze_solve

maze_solve:	$(OBJECTS)  $(HEADERS)
		g++ -o maze_solve $(OBJECTS)

clean:
	rm -f *.o maze_solve

.SUFFIXES: .cpp.o

.cpp.o: $(HEADERS)
	g++ $(CFLAGS) -c $*.cpp
