#ifndef _PPM_Primitive
#define _PPM_Primitive
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
using namespace std;

typedef struct {
  int rows;
  int cols;
  int maxpixel;
  vector <int> pixels;
} PPM_Primitive;

PPM_Primitive *read_PPM(string fn);
void write_PPM(PPM_Primitive *p, string fn);
PPM_Primitive *rotate_PPM(PPM_Primitive *p);
PPM_Primitive *crop_PPM(PPM_Primitive *p, int width, int height, int from_left, int from_top);

#endif
