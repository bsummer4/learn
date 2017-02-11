all: ppm-copy ppm-rotate ppm-crop

clean:
	rm -f *.o ppm-copy ppm-rotate ppm-crop

.SUFFIXES: .cpp .o
.cpp.o: g++ -O3 -c $*.cpp 

ppm-prim.o: ppm-prim.h
ppm-copy.o: ppm-prim.h
ppm-rotate.o: ppm-prim.h
ppm-crop.o: ppm-prim.h

ppm-copy: ppm-copy.o ppm-prim.o
	g++ -O3 -o ppm-copy ppm-copy.o ppm-prim.o

ppm-rotate: ppm-rotate.o ppm-prim.o
	g++ -O3 -o ppm-rotate ppm-rotate.o ppm-prim.o

ppm-crop: ppm-crop.o ppm-prim.o
	g++ -O3 -o ppm-crop ppm-crop.o ppm-prim.o

