#include "ppm-prim.h"

main(int argc, char **argv)
{
  PPM *p, *p2;

  if (argc != 3) {
    fprintf(stderr, "usage: ppm-rotate inputfile outputfile\n");
    exit(1);
  }
  
  p = read_PPM(argv[1]);
  p2 = rotate_PPM(p);
  write_PPM(p2, argv[2]);
  exit(0);
}
