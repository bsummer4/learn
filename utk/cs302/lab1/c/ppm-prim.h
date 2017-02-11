#ifndef _PPM_Primitive
#define _PPM_Primitive
#include <iostream>
#include <fstream>
using namespace std;

typedef struct {
  int rows;
  int cols;
  int maxpixel;
  int *pixels;
} PPM;

PPM *read_PPM(char* fn);
void write_PPM(PPM *p, char *fn);
PPM *rotate_PPM(PPM *p);
PPM *crop_PPM(PPM *p, int width, int height, int from_left, int from_top);

#endif
