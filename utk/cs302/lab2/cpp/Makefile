CC= g++
CFLAGS= -Wall -ansi -pedantic -g
OBJECTS= album.o artist.o io.o main.o time.o track.o misc.o
HEADERS= lab2.h

all:	lab2 lib_info

lib_info: lab2
	ln -f lab2 lib_info

lab2:	$(OBJECTS)
	$(CC) -o lab2 $(OBJECTS)

clean:
	rm -f lab2 *.o lib_info

album.o: album.cpp
	$(CC) $(CFLAGS) -c album.cpp

artist.o: artist.cpp
	$(CC) $(CFLAGS) -c artist.cpp

io.o: io.cpp
	$(CC) $(CFLAGS) -c io.cpp

main.o: main.cpp
	$(CC) $(CFLAGS) -c main.cpp

time.o: time.cpp
	$(CC) $(CFLAGS) -c time.cpp

track.o: track.cpp
	$(CC) $(CFLAGS) -c track.cpp

misc.o: misc.cpp
	$(CC) $(CFLAGS) -c misc.cpp
