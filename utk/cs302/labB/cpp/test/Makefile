OBJECTS= dond.o bigzigzag.o
CFLAGS= -pedantic -Wall -O3

all: dond bigzigzag

dond:	dond.o
	g++ $(CFLAGS) -o dond dond.o

bigzigzag:	bigzigzag.o
		g++ $(CFLAGS) -o bigzigzag bigzigzag.o

clean:
	rm -f bigzigzag dond $(OBJECTS)

.SUFFIXES:	.cpp.o

.cpp.o:	$(HEADERS)

	g++ $(CFLAGS) -c $*.cpp