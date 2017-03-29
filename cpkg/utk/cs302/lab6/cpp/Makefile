SORTS= quick_2_sort merge_1_sort
HEADERS= sort.h

all: $(SORTS)

quick_2_sort: quick_sort.o sort_driver.o $(HEADERS)
	g++ -o quick_2_sort quick_sort.o sort_driver.o


merge_1_sort: merge_sort.o sort_driver.o $(HEADERS)
	g++ -o merge_1_sort merge_sort.o sort_driver.o

clean:
	rm -f *.o $(SORTS)

.SUFFIXES: .cpp.o

.cpp.o: $(HEADERS)
	g++ -c $*.cpp