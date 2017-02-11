all: roadtest

clean:
	rm -f *.o roadtest

.SUFFIXES: .cpp .o

.cpp.o: g++ -c $*.cpp 

roadtest: roadtest.o roadconfig.o road.o light.o
	g++ -o roadtest roadtest.o roadconfig.o road.o light.o

road.o: light.h road.h
light.o: light.h road.h
roadtest.o: roadconfig.h
roadconfig.o: roadconfig.h road.h light.h
