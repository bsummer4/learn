/*****************************************************************************/
// misc.cpp Implements misc. utillity functions for lab2.
/*****************************************************************************/
#include "lab2.h"


/** Copy a string, replacing underscores with spaces.  
******************************************************************************/
string removeUnderscores(string input) {
  int len = input.length();
  char* result = strdup(input.c_str());

  /** Replace the underscores with spaces.  */  
  for (int ii=0; ii<len; ii++)
    if (result[ii] == '_')
      result[ii] = ' ';
  
  /** Convert char* to string.  */
  return string(result);
}
