OBJECTS= city_map.o city_map_base.o dijsdtra.o
HEADERS= city_map.h
CFLAGS= -O3

all:	city_map

city_map:	$(OBJECTS) $(HEADERS)
		g++ -o city_map $(OBJECTS)

clean:
	rm -f city_map *.o

.SUFFIXES:	.cpp.o

.cpp.o: $(HEADERS)
	g++ $(CFLAGS) -c $*.cpp
