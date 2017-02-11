/*****************************************************************************/
// main.cpp: Main loop for lab1.  See the following link for what it is
// supposed to do.
// "http://www.cs.utk.edu/~plank/plank/classes/cs302/Labs/Lab2/" 
/*****************************************************************************/
#include "lab2.h"

#define NUM_ARGS 1 // number of arguments this program should be
		   // given.

/** Prototypes local to this file.  */
inline char** handleArgs(int argc, char** argv);


/** Open a file, read a bunch of information about music files, and
    output it to the stdout.
******************************************************************************/
int main(int argc, char** argv) {
  char** arguments;
  ifstream inputFile;

  arguments = handleArgs(argc, argv);
  inputFile.open(arguments[0]);
  write_albums(read_file(&inputFile), &cout);
  
  return 0;
}


/** Makes sure that there are the correct number of argumnets, and
    return all of them except the programs name.
******************************************************************************/
inline char** handleArgs(int argc, char** argv) {
  if (argc != NUM_ARGS+1) {
    cerr << "usage " << argv[0] << " input\n";
    exit(1);
  }
  
  return argv+1;
}
