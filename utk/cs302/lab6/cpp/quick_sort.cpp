/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab5
  Fall 2008
  quick_sort.cpp

  Implements the quick sort algorithm in the function quick_sort which
  is callable through jsort;

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab6/
******************************************************************************/


#include "sort.h"



/** Print each item in the vector.  

    It's a shame I have to have this in two places, but the inline
    doesn't work if it's in another .cpp file.
******************************************************************************/
inline void print_vector(vector <double> *v) {
  for (int ii=0; ii < v->size(); ii++)
    cout <<  " " <<(*v)[ii];
  cout << endl;
}


/** Swaps the valus at indecies x and y for v.  
******************************************************************************/
inline void vswap(vector <double> *v, int x, int y) {
  double temp = (*v)[x];
  (*v)[x] = (*v)[y];
  (*v)[y] = temp;
}




/** Find the median of the start, middle, and end points in the
    section of the vector we are dealing with.  Then swap that element
    with the start one.

    The comments in this section lay out the case analisis I used when
    coming up with this algorithm.  The are layed out order from
    smallest to largest and named sequentually from their position in
    the vector.  For example:

    let x be the element at start, y at middle, and z end;
    if x < y < z then we say: X Y Z
    if y < x < z then we say: Y X Z

    I hope that's clear.
******************************************************************************/
inline void setup_pivot(vector <double> *v, int start, int size) {
  int middle = size / 2 + start;
  int end = start + size - 1;

  if((*v)[start] < (*v)[middle]) {  // X < Y

    if((*v)[middle] < (*v)[end])    // Y < Z
      vswap(v, start, middle);      // pos is: X Y Z

    else if((*v)[start] < (*v)[end])// X < Z (Z < Y b/c of the else)
      vswap(v, start, end);         // pos is: X Z Y

    return;                         // other case is: Z X Y, but we do
				    // nothing: x is already at start
  }
  
  if((*v)[end] < (*v)[middle]) // Z < Y (Y < X b/c of the else)
    vswap(v, start, middle);   // pos is Z Y X
  
  else if((*v)[end] < (*v)[start]) // Z < X (b/c of else: Y < X and Z < Y)
    vswap(v, start, end);          // pos is: Y Z X

  return; // other case is: Y X Z, but we do nothing: x is already at
	  // start
}



/** Partition a segment of the vector.  Chooses a pivot, then arranges
    the section of the vector as follows: all items<pivot, pivot, all
    items>pivot.  Finally return the new location of the pivot.
******************************************************************************/
inline int partition(vector <double> *v, int start, int size) {
  int left = start + 1;
  int right = start + size - 1;
  int pivot_index;
  double pivot;

  /** Chooses a pivot and move it to the first element (start).  */
  setup_pivot(v, start, size);
  pivot = (*v)[start];
  
  /** Perform the partition.  */
  while(1) {

    /** Move indexes onto values that need to be swapped.  */
    while(left < size+start && (*v)[left] < pivot) left++;
    while(right > 0 && (*v)[right] > pivot) right--;

    /** Swap unless done.  */
    if(left > right) break;
    vswap(v, right, left);
  }
  
  /** Move the pivot into the center. **/
  vswap(v, right, start);
  pivot_index = right;
  
  /** Print post-partition log.  */
  printf("After Partition: Start=%d Size=%d. Pivot at %d.",
	 start, size, pivot_index);
  print_vector(v);
  
  return pivot_index;
}


/** Sorts a vector of doubles (modifies it) using the quicksort
    algorithm.
******************************************************************************/
void quick_sort(vector <double> *v, int start, int size) {
  printf("Sort called:     Start=%d Size=%d.            ", start, size);
  print_vector(v);

  if(size < 2) return; // Already sorted
  
  /** Simply sort lists of just two elements. */
  if (size == 2) {
    if ((*v)[start] > (*v)[start+1])
      vswap(v, start, start+1);
    return;
  }
  
  int pivot = partition(v, start, size);
  
  if(size == 3) return; // Sorted by partitioning

  quick_sort(v, start, pivot-start);
  quick_sort(v, pivot+1, (size-(pivot-start))-1);
}


/** There is no need for the calling function to know that it needs to
    pass 0 and size. */
void jsort(vector <double> *v) {
  quick_sort(v, 0, v->size());
}
