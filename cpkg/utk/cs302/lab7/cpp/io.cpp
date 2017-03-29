/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab7
  Fall 2008
  io.cpp

  Deals with reading in the maze and printing out a solution.  

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab7/
******************************************************************************/


#include "lab7.h"


/** Makes a maze from an input stream.  Outputs the maze
    information.  
******************************************************************************/
Maze *read_maze(istream &inputStream) {
  string tmp;
  int cols, rows;
  
  inputStream >> tmp >> rows >> tmp >> cols;
  cout << "ROWS " << rows << " COLS " << cols << endl;
  Maze* maze = new Maze(rows,cols);
  
  int cell1, cell2;
  while(inputStream >> tmp >> cell1 >> cell2) {
    cout << "WALL " << cell1 << " " << cell2 << endl;
    maze->add_wall(cell1, cell2);
  }
  
  return maze;
}


/** Prints the result of a solution to a maze.  */
void print_solution(vector <int> *solution) {
  if(!solution) return;

  /** Print vector backwards.  */
  for(int ii=solution->size() - 1; ii >= 0; ii--)
    cout << "PATH " << (*solution)[ii] << endl;
}
