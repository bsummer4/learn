/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab7
  Fall 2008
  maze.cpp

  Implements methods for the maze class.  Builds and solves the maze,
  etc.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab7/
******************************************************************************/


#include "lab7.h"


/** Makes an empty maze with size (rows x cols).  */
Maze::Maze(int rows_, int cols_) {
  rows = rows_;
  cols = cols_;
  size = rows * cols;

  nodes.resize(size);

  /** Add edges between all adjacent nodes.  */
  for(int ii=0; ii<size; ii++) add_adj(ii);
}


/** Add a wall between two cells in the maze.  */
void Maze::add_wall(int cell1, int cell2) {  

  /** Remove edges between the two cells.  */
  nodes[cell1].erase(cell2);
  nodes[cell2].erase(cell1);
}


/** Find a path from the bottom-right node to 'index. */
vector <int> *Maze::solve(int index) {
  Node* node = &(nodes[index]); // node at 'index
  
  if(node->visited) return NULL;
  node->visit();
  
  // We're at the end of the maze
  if(index == size - 1) return new vector <int> (1, index);
  
  /** Iterate over all edges, calling ourselves on them.  If one of
      them returns a solution: push our index on it and return.  */
  Node::reverse_iterator nit; // It is not important that this is
			      // in reverse.
  vector <int> * result;
  for(nit = node->rbegin(); nit != node->rend(); nit++)
    if((result = solve(*nit)) != NULL) {
      result->push_back(index);
      return result;
    }
  
  // There is no solution from here.
  return NULL;
}


/** Get index into maze from row, col.  */
inline int Maze::oneD(int row, int col) {
  return col + (row * cols);
}


/** Adds the all the adjacent Nodes to a certain nodes' edges */
inline void Maze::add_adj(int index) {
  Node* node = &(nodes[index]);
  int col = index % cols;
  int row = index / cols;
  
  // Add all <= four adjacent nodes
  if (col > 0) node->insert(oneD(row, col-1)); // left
  if (row > 0) node->insert(oneD(row-1, col)); // up
  if (col < cols-1) node->insert(oneD(row, col+1)); // right
  if (row < rows-1) node->insert(oneD(row+1, col)); //down
}
