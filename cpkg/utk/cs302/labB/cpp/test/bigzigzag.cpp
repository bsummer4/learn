/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 LabB
  Fall 2008
  bigzigzag.cpp
  
  The whole bigzigzag program.  Reads the file into a vector and
  prints the longest zig zag pattern in the vector.  Uses memoization.
  
  See:
  http://www.cs.utk.edu/~cs302/Labs/LabB/
******************************************************************************/


#include <map>
#include <iostream>
#include <sstream>
#include <vector>
using namespace std;

typedef map <pair <int, bool>, vector <double> *> Cache;

vector <double> *bigzigzag(vector <double> * numbers);
void read_vector(vector <double>*);


/** Reads a vector of doubles from standard input and then finds the
    largest zig zag pattern in the sequence.  Finally, it prints
    it. 
*******************************************************************************/
int main(int argc, char** argv) {
  vector <double> numbers;

  read_vector(&numbers);

  vector <double> *result = bigzigzag(&numbers);

  for (int ii = 0; ii < (int)result->size(); ii++)
    cout << result->at(ii) << endl;

  delete result;

  return 0;
}

/** The recursive part of bigzigzag.  */
vector <double> *bigzigzag_(vector <double> * numbers, uint start, bool isUp, uint count, Cache & cache) { 
  pair <int, bool> index = make_pair(start, isUp);

  // Memoization: Check the cache
  if(cache.count(index)) return cache[index];

  /** The longest zig zag sequence starting with 'start and in the
      'isUp direction.  */
  vector <double> *longest = new vector <double> ();
  vector <double> *freeme = longest;
  
  /** Find the longest of all the zig zags that start on indecies
      after 'start and move in the opposite direction of isUp.  */
  for (uint ii = start + 1; ii < numbers->size(); ii++) {
    if (numbers->at(ii) <= numbers->at(start) && isUp) continue;
    if (numbers->at(ii) >= numbers->at(start) && !isUp) continue;

    // biggest zig zag starting at the current index 'ii
    vector <double> *temp = bigzigzag_(numbers, ii, !isUp, count+1, cache);

    // keep track of the longest
    if (temp->size() > longest->size()) longest = temp;
  }
  
  // Memoization.  Insert the vector into the cache.
  vector <double> *result = new vector <double> (*longest);
  result->insert(result->begin(), numbers->at(start));
  
  if (freeme != longest) delete freeme;
  return cache[index] = result;
}


/** Calls the recursive bigzigzag function from both directions
    starting at the first index.  
******************************************************************************/
inline vector <double> *bigzigzag(vector <double> *numbers) {
  Cache cache;
 
  vector <double> *up = bigzigzag_(numbers, 0, true, 1, cache);
  vector <double> *down = bigzigzag_(numbers, 0, false, 1, cache);

  if (up->size() >= down->size()) return up;
  return down;
}


/** Reads a vector from stdin to the numbers vectir */
inline void read_vector(vector <double> *numbers) {
  double temp;
  while(cin >> temp) numbers->push_back(temp);
}
