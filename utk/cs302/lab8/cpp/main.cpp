/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab8
  Fall 2008
  main.cpp

  Functions for controling the program (main) and doing io.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab8/
******************************************************************************/

#include "lab8.hpp"



/** It's pretty obvious.  */
int main() {
  int* x, y;
  
  Graph* graph = read_graph(cin);
  print_solution(graph->min_span_tree());
  
  return 0;
}


/** Reads a list of edges from the file and creates a graph from them.  

    The file is just a series of white-space-separated edges.  edges
    are of the form "node1 node2 weight".
******************************************************************************/
Graph* read_graph(istream &input) {
  Graph* result = new Graph();
  string n1, n2;
  double w;
  
  /** Add each edge in the file to the graph.  */
  while(input >> n1 >> n2 >> w)
    result->add_edge(n1, n2, w);
  
  return result;
}



/** Prints a minimum vector representing the edges of a minimum
    spanning tree of a graph.

    Output is of the form "Edge [weight,from,to]".
******************************************************************************/ 
void print_solution(vector <Edge*> *sol) {
  double weight = 0; // total weight of edges in solution
  vector <Edge*>::iterator sit;

  if (!sol) {
    cout << "Unconnected graph" << endl;
    exit (0);
  }

  
  /** Print each edge in the solution.  */
  for(sit = sol->begin(); sit != sol->end(); sit++) {
    weight += (*sit)->weight;
    cout << "Edge [" << (*sit)->weight;
    cout << "," << (*sit)->from->name;
    cout << "," << (*sit)->to->name;
    cout << "]" << endl;
  }
  
  cout << "Total Weight: " << weight << endl;
}

