/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 LabB
  Fall 2008
  dond.cpp
  
  The whole Dice or no Dice program.  Computes the probabillity of
  winning based on the number of rolls left, the number sides on the
  dice, and the previous roll.  Uses memoization.
  
  See:
  http://www.cs.utk.edu/~cs302/Labs/LabB/
******************************************************************************/


#include <map>
#include <iostream>
#include <sstream>
using namespace std;

typedef map <pair <uint, uint>, double> Cache;

void handle_args(int, char**, uint*, uint*, uint*);
double dond(uint, uint, uint);
bool is_adj(uint n, uint m);


// The main function...
int main(int argc, char** argv) {
  uint sides, rolls, last_roll;

  // If last_roll is -1 then handle args returns INT_MAX instead.
  handle_args(argc, argv, &sides, &rolls, &last_roll);

  cout << dond(sides, rolls, last_roll) << endl;

  return 0;
}


/** Recursively calulates the probability of winning the dond game
    using memoization.  */
double dond_(uint sides, uint remaining_rolls, uint last_roll, Cache &cache) {
  double result = 0;
  pair <uint, uint> index(remaining_rolls, last_roll);
  
  // Base case.
  if (remaining_rolls == 0) return 1;
  
  // Check the cache.
  if(cache.count(index)) return cache[index];
  
  // Sum of the probabilities for all the next remaining_rolls.  
  for(uint roll = 0; roll < sides; roll++)
    if (!is_adj(roll, last_roll)) // INT_MAX is never is_adj to roll.
      result += dond_(sides, remaining_rolls - 1, roll, cache);
  
  // Return the average.
  return cache[index] = result / (double) sides;
}



// Just sets up the cache for the recursive dond.
inline double dond(uint sides, uint num_rolls, uint first_roll) {
  static uint cache_sides=UINT_MAX;
  static Cache cache;
  
  // This isn't needed for this lab, but it makes the function more general.
  if (cache_sides != sides) {
    cache_sides = sides;
    cache.clear();
  }
  
  return dond_(sides, num_rolls, first_roll, cache);
}


// Handle the arguments for main
inline void handle_args(int argc, char** argv, uint *sides, uint *rolls, uint *last_roll) {
  if (argc != 4) exit(-1);
  
  stringstream stream;
  for (uint ii=1; ii<4; ii++) stream << argv[ii] << " ";
  stream >> *sides >> *rolls >> *last_roll;
}


// Are the two numbers adjacent?
inline bool is_adj(uint n, uint m) {
  return (n == m+1 || n == m || n+1 == m);
}
