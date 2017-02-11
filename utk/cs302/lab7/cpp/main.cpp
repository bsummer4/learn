/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab7
  Fall 2008
  main.cpp

  It is two lines of code....

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab7/
******************************************************************************/

#include "lab7.h"

int main() {
  Maze *maze = read_maze(cin);
  print_solution(maze->solve());
}
