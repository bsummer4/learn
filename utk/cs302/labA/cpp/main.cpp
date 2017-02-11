/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 LabA
  Fall 2008
  main.cpp

  Handles the main function and all io.

  See:
  http://www.cs.utk.edu/~cs302/Labs/LabA/
******************************************************************************/


#include "main.hpp"
#include "dice.hpp"


Dice *read_dice(istream &input);
vector <string> *read_words(istream &input);


/** The main loop.  Read in both files, build the dice solver object,
    and solve it for each word, printing the solution.
******************************************************************************/
int main(int argc, char** argv) {

  /** Read dice.  */
  ifstream dice_file(argv[1]);
  Dice *dice = read_dice(dice_file);
  dice_file.close();
  
  /** Read words.  */
  ifstream words_file(argv[2]);
  vector <string> *words = read_words(words_file);
  words_file.close();
  
  /** For each word, solve the network flow problem, and print the
      solution.  */
  for(uint ii=0; ii < words->size(); ii++) {
    string word = words->at(ii);

    /** Solve the dice problem for the current word.  */
    vector <int> *solution = dice->solve(word);
      
    /** If there is no solution for the current word.  */
    if(!solution) {
      cout << "Cannot spell " << word << endl;
      continue;
    }
    
    /** Print the solution.  */
    for(uint jj = 0; jj < solution->size(); jj++) {
      cout << solution->at(jj);
      cout << ((jj+1 == solution->size()) ? ":" : ","); // print : or ,
    }
    
    cout << " " << word << endl;
    delete solution;
  }

  delete words;
  delete dice;
  return 0;
}


/** Builds up a dice graph for all string (character lists) in the
    file.  
******************************************************************************/
inline Dice *read_dice(istream &input) {
  Dice* result = new Dice();
  
  string die;
  while(input >> die) result->add_dice(die);
  return result;
}


/** Returns a vector of words from a stream.  */
inline vector <string> *read_words(istream &input) {
  vector <string> *result = new vector <string>;
  
  string word;
  while(input >> word) result->push_back(word);
  return result;
}
