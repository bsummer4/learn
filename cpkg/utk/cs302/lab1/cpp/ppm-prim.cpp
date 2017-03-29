/******************************************************************************
ppm-prim.c
Lab1 for cs302
Benjamin Summers
September 4, 2008

This file implements four functions that operate on a PPM_Primitive.
read_PPM, write_PPM, crop_PPM, and rotate_PPM.  See ppm-prim.h or
"http://www.cs.utk.edu/~plank/plank/classes/cs302/Labs/Lab1/".
******************************************************************************/

#include "ppm-prim.h"
#include <sstream>


typedef vector <int> matrixIndicies;

/* Prototypes for utility functions.  */
int twoDeeToOne(int ii, int jj, int cols);
matrixIndicies oneDeeToTwo(int index, int cols);
string badPixel(int ii, int cols);


/** Read ppm imformation from a file.
******************************************************************************/
PPM_Primitive* read_PPM(string fn) {
  PPM_Primitive* result = new PPM_Primitive;
  ifstream inputFile(fn.c_str());

  /**  Read from the file and verify all input, complaing nicly on
       errors.  */
  try {   // The try/catch should be in main, but I can't edit it.

    /** Header Data. */
    string p3Str;
    inputFile >> p3Str;
    if(inputFile.fail() || p3Str != string("P3"))
      throw string("first word is not P3");
    
    inputFile >> result->cols;
    if(inputFile.fail() || result->cols < 0)
      throw string("bad column specification");

    inputFile >> result->rows;
    if(inputFile.fail() || result->rows < 0)
      throw string("bad row specification");
    
    inputFile >> result->maxpixel;
    if(inputFile.fail() || result->maxpixel < 0)
      throw string("bad maxpixel specification");
    
    /** Pixel Data */
    int currentPixel=0; // The number of pixel that we are adding
    while(1) {
      int currentInt; // The current word from the file.
      inputFile >> currentInt;
      
      if(inputFile.eof()) break;
      
      if(inputFile.fail() || currentInt < 0 || currentInt > result->maxpixel)
	throw badPixel(currentPixel, result->cols);

      result->pixels.push_back(currentInt);
      currentPixel++;
    }
    
    /** Make sure we've gotten the right number of pixels.  */
    /** Too many: */
    int numPixels = result->pixels.size();
    if(numPixels > result->rows*result->cols*3)
      throw string("Extra stuff at the end of the file");
    
    /** Not enough: */
    if(numPixels < result->rows*result->cols*3)
      throw badPixel(currentPixel, result->cols);
    
    return result;
 
    /** Catch errors and handle error formating.  */
  } catch(string error) {
    cerr << "Bad PPM file - " << error << endl;
    exit(1);
  }
}


/** Write out *p* as a valid ppm file to the file *fn*.

    Format
    ------
    The header, ('P3' rows cols maxpixel), is on the first
    line. Following that, each pixel is on its own line.
******************************************************************************/
void write_PPM(PPM_Primitive *p, string fn) {
  ofstream outputFile(fn.c_str());
  
  /** Header.  */
  outputFile << "P3" << " ";
  outputFile << p->cols << " ";
  outputFile << p->rows << " ";
  outputFile << p->maxpixel << " ";
  
  /** Pixels.  */
  int numPixels = p->pixels.size();
  for(int ii=0; ii<numPixels; ii++) {
    if ((ii%3)==0) outputFile << endl; // A new pixel
    outputFile << p->pixels[ii] << " ";
  }
  
  /** If anything went wrong.  */
  if(outputFile.bad())
    throw string("Failed to write to ") + fn;
  outputFile.close();
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
PPM_Primitive* rotate_PPM(PPM_Primitive *p) {
  PPM_Primitive *result;
  
  /** Copy the header, swapping the rows and cols.  */
  result = new PPM_Primitive;
  result->rows = p->cols;
  result->cols = p->rows;
  result->maxpixel = p->maxpixel;
  
  /** Resize pixels vector.  */
  result->pixels.resize(p->pixels.size());

  /** Rotate the pixels.  */
  int newIndex = 0;
  for(int ii=0; ii<(p->cols); ii++) { // [0, cols)
    for(int jj=(p->rows)-1; jj>=0; jj--) { // (cols, 0]
      int oldIndex = 3*twoDeeToOne(ii, jj, p->cols);

      /** Copy the pixel. (three ints)  */
      memcpy(&(result->pixels[newIndex]),
	     &(p->pixels[oldIndex]),
	     3*sizeof(int));
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
PPM_Primitive *crop_PPM(PPM_Primitive *p,
			int width,
			int height,
			int from_left,
			int from_top) {

  PPM_Primitive *result = new PPM_Primitive;

  /** Header.  */
  result->cols = width;
  result->rows = height;
  result->maxpixel = p->maxpixel;
  
  /** Pixels.  */
  result->pixels.resize(width*height*3);
  for(int ii=0; ii<width; ii++) {
    for(int jj=0; jj<height; jj++) {
      int oldIndex = 3*twoDeeToOne(ii+from_left, jj+from_top, p->cols);
      int newIndex = 3*twoDeeToOne(ii, jj, width);
      
      /** Copy the pixel. (three ints)  */
      memcpy(&(result->pixels[newIndex]),
	     &(p->pixels[oldIndex]),
	     3*sizeof(int));
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
int twoDeeToOne(int ii, int jj, int cols) {
  return (cols*jj)+ii;
}


/** Return the indexes of a 2D array (as a vector=(col,row)) given a
    and index into a 1D array, and the number of colomns in the 2D
    one.  
******************************************************************************/
matrixIndicies oneDeeToTwo(int index, int cols) {
  matrixIndicies result(2);
  result[0] = index / cols; // col
  result[1] = index % cols; // row
  
  return result;
}


/** Return an error string for a bad pixel given the index into the
    pixel array and the number of columns in the image.  
******************************************************************************/
string badPixel(int ii, int cols) {
  ostringstream err;
  matrixIndicies index = oneDeeToTwo(ii/3, cols);
  err << "pixel in row " << index[0] << " ";
  err << "column " << index[1] << " ";
  err << "#" << ii%3 << " "; 
  err << "incorrect";
  return err.str();
}
