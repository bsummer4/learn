#include "ppm-prim.h"

main(int argc, char **argv)
{
  PPM *p;

  if (argc != 3) {
    fprintf(stderr, "usage: ppm-copy inputfile outputfile\n");
    exit(1);
  }
  
  p = read_PPM(argv[1]);
  write_PPM(p, argv[2]);
  exit(0);
}

