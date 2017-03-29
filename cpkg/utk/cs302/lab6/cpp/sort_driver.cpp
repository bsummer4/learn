/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab5
  Fall 2008
  sort_driver.cpp

  Slightly modified version of the one we were given.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab6/
******************************************************************************/


#include "sort.h"


void usage(char *s)
{
  cerr << "usage: sort_driver size iterations seed double-check(yes|no) print(yes|no)\n";
  if (s != NULL) cerr << s << endl;
  exit(1);
}

main(int argc, char **argv)
{
  int size;
  int iterations, it;
  int seed;
  int dc;
  int i;
  int print;
  vector <double> *v;

  if (argc != 6) usage(NULL);

  if (sscanf(argv[1], "%d", &size) != 1 || size <= 0) usage("Bad size\n");
  if (sscanf(argv[2], "%d", &iterations) != 1 || iterations < 0) usage("Bad iterations\n");
  if (sscanf(argv[3], "%d", &seed) != 1) usage("Bad seed\n");
  if (strcmp(argv[4], "yes") == 0) {
    dc = 1;
  } else if (strcmp(argv[4], "no") == 0) {
    dc = 0;
  } else { 
    usage("Bad double-check\n");
  }
  if (strcmp(argv[5], "yes") == 0) {
    print = 1;
  } else if (strcmp(argv[5], "no") == 0) {
    print = 0;
  } else { 
    usage("Bad print\n");
  }

  srand48(seed);

  v = new vector <double> (size);

  for (it = 0; it < iterations; it++) {
    for (i = 0; i < size; i++) (*v)[i] = drand48();
    jsort(v);
    if (dc) {
      for (i = 1; i < size; i++) {
        if ((*v)[i] < (*v)[i-1]) {
          cout << "Sorting Error during iteration " << it << ": v[" << i-1 <<
           "] = " << (*v)[i-1] << " and v[" << i << "] = " << (*v)[i] << endl;
          exit(1);
        }
      }
    }
  }
}
