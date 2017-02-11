#include "ppm-prim.h"

main(int argc, char **argv)
{
  PPM *p, *p2;
  int width, height, from_left, from_top;

  if (argc != 7) {
    fprintf(stderr, "usage: ppm-crop inputfile outputfile width height from_left, from_top\n");
    exit(1);
  }
  
  p = read_PPM(argv[1]);

  width = atoi(argv[3]);
  if (width <= 0) {
    fprintf(stderr, "Bad width specification.\n");
    exit(1);
  }

  height = atoi(argv[4]);
  if (height <= 0) {
    fprintf(stderr, "Bad height specification.\n");
    exit(1);
  }

  if (sscanf(argv[5], "%d", &from_left) != 1 || from_left < 0) {
    fprintf(stderr, "Bad from_left specification.\n");
    exit(1);
  }

  if (sscanf(argv[6], "%d", &from_top) != 1 || from_top < 0) {
    fprintf(stderr, "Bad from_top specification.\n");
    exit(1);
  }

  if (width + from_left > p->cols) {
    fprintf(stderr, "Width + from_left is too large\n");
    exit(1);
  }

  if (height + from_top > p->cols) {
    fprintf(stderr, "Height + from_top is too large\n");
    exit(1);
  }

  p2 = crop_PPM(p, width, height, from_left, from_top);
  write_PPM(p2, argv[2]);
  exit(0);
}
