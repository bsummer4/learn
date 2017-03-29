/******************************************************************************
ppm-prim.c
Lab1 for cs302
Benjamin Summers
September 4, 2008

This file implements four functions that operate on a PPM.
read_PPM, write_PPM, crop_PPM, and rotate_PPM.  See ppm-prim.h or
"http://www.cs.utk.edu/~plank/plank/classes/cs302/Labs/Lab1/".
******************************************************************************/

#include "ppm-prim.h"
#include <sstream>
#include <stdio.h>
#include <stdlib.h>


/* Prototypes for utility functions.  */
int twoDeeToOne(int ii, int jj, int cols);
int *oneDeeToTwo(int index, int cols);
char *badPixel(int ii, int cols);

inline void error(char* message) {
  fprintf(stderr, "Bad PPM file - %s\n", message);
  exit(1);
}

/** Read ppm imformation from a file.
******************************************************************************/
PPM* read_PPM(char* fn) {
  PPM* result = (PPM*) malloc(sizeof (PPM));
  std::ifstream inputFile(fn);

  /** Header Data. */
  std::string p3Str;
  inputFile >> p3Str;
  if(inputFile.fail() || strcmp(p3Str.c_str(), "P3"))
    error("first word is not P3");
    
  inputFile >> result->cols;
  if(inputFile.fail() || result->cols < 0)
    error("bad column specification");

  inputFile >> result->rows;
  if(inputFile.fail() || result->rows < 0)
    error("bad row specification");
    
  inputFile >> result->maxpixel;
  if(inputFile.fail() || result->maxpixel < 0)
    error("bad maxpixel specification");

    
  int size = result->rows * result->cols * 3;
  result->pixels = (int*) malloc(sizeof(int)*size);
    
  /** Pixel Data */
  int ii=0; // The number of pixel that we are adding
  while(1) {
    int currentInt; // The current word from the file.
    inputFile >> currentInt;
      
    if(inputFile.eof()) break;
      
    if(inputFile.fail() || currentInt < 0 || currentInt > result->maxpixel)
      badPixel(ii, result->cols);

    result->pixels[ii] = currentInt;
    ii++;
    if(ii > size) error("Extra stuff at the end of the file");
  }

  /** Not enough: */
  if(ii < result->rows*result->cols*3)
    badPixel(ii, result->cols);
  
  return result;
}


/** Write out *p* as a valid ppm file to the file *fn*.

    Format
    ------
    The header, ('P3' rows cols maxpixel), is on the first
    line. Following that, each pixel is on its own line.
******************************************************************************/
void write_PPM(PPM *p, char* fn) {
  FILE* output = fopen(fn, "w");

  /** Header.  */
  fprintf(output, "P3  %d %d %d\n", p->cols, p->rows, p->maxpixel);

  /** Pixels.  */
  int numPixels = p->rows * p->cols * 3;
  for(int ii = 0; ii < numPixels; ii++) {
    fprintf(output, (ii%3==2?"%d\n":"%d "), p->pixels[ii]);
  }
  
  fclose(output);
}


/** Return a ppm image that represents the passed image except rotated
    90 degrees to the right.
    
    Algorithm
    ---------
    * Create a new ppm File
    * Copy the headerm swappin the rows and cols
    
    * Iterate through the cols, starting at the first(left) one
      - Iterate through the rows, starting at the last(bottom) one.
        * Append the current pixel to our new image.
******************************************************************************/
PPM* rotate_PPM(PPM *p) {

  /** Copy the header, swapping the rows and cols.  */
  PPM* result = (PPM*) malloc(sizeof (PPM));
  result->rows = p->cols;
  result->cols = p->rows;
  result->maxpixel = p->maxpixel;
  
  /** Resize pixels vector.  */
  int size = p->cols * p->rows * 3;
  result->pixels = (int*) malloc(sizeof(int)*size);

  /** Rotate the pixels.  */
  int newIndex = 0;
  for(int ii=0; ii<(p->cols); ii++) { // [0, cols)
    for(int jj=(p->rows)-1; jj>=0; jj--) { // (cols, 0]
      int oldIndex = 3*twoDeeToOne(ii, jj, p->cols);

      /** Copy the pixel. (three ints)  */
      memcpy(result->pixels + newIndex,
	     p->pixels + oldIndex,
	     3 * sizeof(int));
      newIndex += 3;
    }
  }
  
  return result;
}


/** Return a cropped ppm image given a set of dimentions and an offset
    from the top and left borders on the passed ppm image.
    
    Algorithm
    ---------
    * Create a new ppm image
    * Create the new header from the old header and the new dimentions
    * Create the new pixels vecter and resize it to the correct size

    * Iterate through the pixels in the _new_ ppm image.
      - Calculate the index in the old ppm image which holds the value
        for the new one.
      - Copy that value into the current index
******************************************************************************/
PPM *crop_PPM(PPM *p,
              int width,
              int height,
              int from_left,
              int from_top) {

  PPM *result = (PPM*) malloc(sizeof (PPM));

  /** Header.  */
  result->cols = width;
  result->rows = height;
  result->maxpixel = p->maxpixel;
  
  /** Pixels.  */
  result->pixels = (int*) malloc(sizeof(int)*width*height*3);
  for(int ii=0; ii<width; ii++) {
    for(int jj=0; jj<height; jj++) {
      int oldIndex = 3 * twoDeeToOne(ii+from_left, jj+from_top, p->cols);
      int newIndex = 3 * twoDeeToOne(ii, jj, width);
      
      /** Copy the pixel. (three ints)  */
      memcpy(&(result->pixels[newIndex]),
	     &(p->pixels[oldIndex]),
	     3 * sizeof(int));
    }
  }
  
  return result;
}


/*****************************************************************************/
// Utility Functions:
/*****************************************************************************/


/** Return an index for a 1D array given the two indexes for a 2D
    array and the number of coloumns.
******************************************************************************/
inline int twoDeeToOne(int ii, int jj, int cols) {
  return (cols*jj)+ii;
}


/** Return the indexes of a 2D array (as a vector=(col,row)) given a
    and index into a 1D array, and the number of colomns in the 2D
    one.  
******************************************************************************/
inline int *oneDeeToTwo(int index, int cols) {
  int* result = (int*) malloc(sizeof (int)*2);
  result[0] = index / cols; // col
  result[1] = index % cols; // row
  
  return result;
}


/** Return an error string for a bad pixel given the index into the
    pixel array and the number of columns in the image.  
******************************************************************************/
inline char *badPixel(int ii, int cols) {
  int* index = oneDeeToTwo(ii/3, cols);
  size_t len = 60;
  char* message = (char*) malloc (sizeof (char)*len);
  snprintf(message, len, "pixel in row %d column %d #%d incorrect", index[0], index[1], ii%3);
  free(index);
  error(message);
}
