OBJECTS= disk.o events.o main.o simulation.o utils.o
HEADERS= lab5.h

all:	raid

raid:	$(OBJECTS)  $(HEADERS)
	g++ -o raid $(OBJECTS)

clean:
	rm -f *.o raid

.SUFFIXES: .cpp.o

.cpp.o: $(HEADERS)
	g++ -c $*.cpp
