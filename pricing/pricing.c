#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define S static
#define SI static inline

// conceptually, this computes the following function:
//   let type PricePt = {units:Int, cost:Int}
//   let type BomSpec = {xmax:Int, resolution:Int, [{quant:Int,offers:[PricePt]}]}
//   pricebom ∷ BomSpec → [[PricePt]

// However, the actual encoding of the data is more complicated.

struct BOM {
	int nitems;
	int *quantity; // length: nitems
	int *noffers;  // length: nitems
	int *units;    // length: sum(noffers)
	double *price; // length: sum(noffers)
	};

struct Part {
	int quantity;
	int noffers;
	int *units;    // length: noffers
	double *price; // length: noffers
	};

typedef struct Part Part;
typedef struct BOM BOM;


// buf must be at least this big: (xmax+1)*sizeof(double)
SI void pricepart
	( int xmax
	, int res
	, Part p
	, double *restrict out
	, double *restrict buf) {

		buf[0] = 0;
		for (int buying=1; buying<=xmax; buying++) {
			double min = INFINITY;
			for (int o=0; o<p.noffers; o++) {
				int units = p.units[o];
				double price = p.price[o];
				int stillneed = buying-units;
				if (stillneed > 0) { price += buf[stillneed]; }
				if (price < min) { min=price; }}
			buf[buying]=min; }

		double increment = (double)xmax / (double)(res-1);
		for (int ii=0; ii<res; ii++) {
			int x = round(increment*ii);   // Nearest neighbor
			if (x > xmax) { x=xmax; }      // This shouldn't happen.
			out[ii] = p.quantity * buf[x]; }}


SI void pricebom (int xmax, int res, BOM b, double *restrict out) {
	double *buf = malloc((xmax+1)*sizeof(double));
	for (int offer=0,item=0; item<b.nitems; offer+=b.noffers[item], item++) {
		double q = b.quantity[item];
		int n = b.noffers[item];
		Part p = {q, n, b.units+offer, b.price+offer};
		pricepart(xmax, res, p, out+(res*item), buf); }

	free(buf); }


SI void plotpricing (int xmax, int res, int nitems, double *pricing) {
	double increment = (double)xmax / (double)(res-1);
	printf("0 0 0\n");
	for (int ii=1; ii<res; ii++) {
		double x = round(increment*ii);
		printf("%lf ", x);
		for (int item=0; item<nitems; item++) {
			int idx = item*res+ii;
			double pr = x==0 ? 0 : pricing[idx]/x;
			printf("%lf%s", pr, (item+1<nitems ? " " : "")); }
		printf("\n"); }}

#define XMAX 100000
#define RES 400
#define NITEMS 100
#define NOFFERS 100

int main (int argc, char **argv) {
	int quantity[NITEMS];
	int noffers[NITEMS];
	int nunits[NOFFERS*NITEMS];
	double price[NOFFERS*NITEMS];
	double buf[RES*NITEMS];

	for (int i=0; i<NITEMS; i++) {
		noffers[i] = NOFFERS;
		quantity[i] = 1;
		for (int j=0; j<NOFFERS; j++) {
			int idx = NOFFERS*i + j;
			nunits[idx] = j+1;
			price[idx] = (j+1)*(0.5+0.5*rand()/RAND_MAX); }}

	pricebom(XMAX, RES, (BOM){NITEMS,quantity,noffers,nunits,price}, buf);
	plotpricing(XMAX, RES, NITEMS, buf); }
