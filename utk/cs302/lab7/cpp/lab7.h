/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab7
  Fall 2008
  lab7.h

  Prototypes and class definitions.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab7/
******************************************************************************/


#include <vector>
#include <set>
#include <iostream>

using namespace std;


/** Just a markable set.  */
class Node: public set <int> {
 public:
  Node() { visited = 0; }
  bool visited;
  void visit() { visited = 1; }
};


/** A maze consists of a vector of Nodes.  You can create an empty
    maze, add walls to one, or solve one.  
******************************************************************************/
class Maze {
 public:
  Maze(int rows_, int cols_);
  void add_wall(int cell1, int cell2);
  vector <int> *solve() { return solve(0); } // a simple wrapper
  
 private:
  int rows, cols, size;
  vector <Node> nodes;
  int oneD(int row, int col);
  void add_adj(int index);
  vector <int> *solve(int index);
};


Maze *read_maze(istream &inputStream);
void print_solution(vector <int> *solution);
