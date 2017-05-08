all: pypricing.so

install: pypricing.so
	cp pypricing.so ../server/scale/pypricing.so

test: pypricing.so
	./test.py > data.txt

	gnuplot -e "\
		set output 'test.png';                      \
		set terminal png;                           \
		plot 'data.txt' using 1:2 with linespoints, \
		     ''         using 1:3 with linespoints; "

clean:
	rm -f o.* *.o *.so data.txt test.png pypricing.c
	rm -rf ./yuck

pricing-in-hs: Main.hs
	ghc -O2 -fllvm Main.hs -o $@

pricing-in-c: pricing.c
	clang -O3 pricing.c -o $@
