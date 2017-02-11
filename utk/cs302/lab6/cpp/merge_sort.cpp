/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab5
  Fall 2008
  merge_sort.cpp

  Implements the merge sort algorithm in the function merge_sort which
  is callable through jsort;

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab6/
******************************************************************************/


#include "sort.h"


/** Print each item in the vector.  
******************************************************************************/
inline void print_vector(vector <double> *v) {
  for (int ii=0; ii < v->size(); ii++)
    cout <<  " " <<(*v)[ii];
  cout << endl;
}


/** Sorts a section of a vector operating on the assumption that both
    halves of the list are sorted.  The passed temp vector must be the
    same size as the vector to be sorted.
******************************************************************************/
void merge(vector <double> *tmp,
	   vector <double> *v,
	   int start,
	   int size,
	   int center,
	   int depth) {

  // These will be modified.  They point to the current element in
  // the left half, the right half, and the temp space respectively.
  int left = start;
  int right = center;
  int sorted = start;

  /** Print call log.  */
  printf("Merging:         Start=%d Size=%d.            ", start, size);
  print_vector(v);

  /** Copies the smallest element from both halves into temp until one
      of the halves runs out of elements.  */
  while ( (left<center) && (right<size+start)) {

    if ((*v)[left] <= (*v)[right]) // left is smaller
      (*tmp)[sorted++] = (*v)[left++];

    else // right is smaller
      (*tmp)[sorted++] = (*v)[right++]; 
  }
  
  
  /** Copy rest of whichever section still has elements in it.  */
  while(left<center)
    (*tmp)[sorted++] = (*v)[left++];
  while(right<size+start)
    (*tmp)[sorted++] = (*v)[right++];
  
  /** Copy relevant section of 'tmp back into 'v.  */
  for(int ii = start; ii < size+start; ii++)
    (*v)[ii] = (*tmp)[ii];
}


/** Sorts a vector of doubles (modifies it) using the mergesort
    algorithm.
******************************************************************************/
void merge_sort(vector <double> *tmp,
		vector <double> *v,
		int start,
		int size,
		int depth)
{
  
  /** Print call log.  */
  printf("Sort called:     Start=%d Size=%d.            ", start, size);
  print_vector(v);

  if (size < 2) return; // Vector is already sorted.
  
  // Makes the index calculation look better.  
  int center = size / 2 + start;
  int first_half = center-start;
    
  // Sort both halves.  
  merge_sort(tmp, v, start, first_half, depth+1);
  merge_sort(tmp, v, center, size-first_half, depth+1);

  // Merge the two sorted halves.  
  merge(tmp, v, start, size, center, depth);
}


/** Wrapper for merge_sort.  */
void jsort(vector <double> *v) {
  int size = v->size(); 

  // set up some temp space for merge_sort.  Will be freed when it
  // goes out of scope.
  vector <double> tmp (size);
  
  merge_sort(&tmp, v, 0, size, 0);
}
