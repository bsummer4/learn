/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 LabA
  Fall 2008
  dice.cpp
  
  Implements the dice class methods.  The dice class is really just an
  interface for a BipartiteGraph.
  
  See:
  http://www.cs.utk.edu/~cs302/Labs/LabA/
******************************************************************************/


#include "main.hpp"
#include "dice.hpp"
#include "graph.hpp"


/** Adds a new word die. A string is used to represent a sequence of characters.  */
void Dice::add_dice(string characters) {
  dice.push_back(characters);
}


/** Build the graph up and connect it's nodes.  */
vector <int> *Dice::solve(string word) {
  BipartiteGraph graph(dice.size(), word.size());

  /** Make connections for each character from every die which contain
      that character.  */
  for (uint letter=0; letter < word.length(); letter++)
    for (uint die=0; die < dice.size(); die++)
      if (dice[die].find(word[letter]) != string::npos) // if character in string:
        graph.connect(die, letter);
  
  return graph.solve();
}
