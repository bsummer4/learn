/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 LabA
  Fall 2008
  graph.cpp

  Implements all the methods for BipartiteGraph

  See:
  http://www.cs.utk.edu/~cs302/Labs/LabA/
******************************************************************************/


#include "graph.hpp"


/** A graph with 'left_size nodes on the left, 'right_size nodes on
    the right, a source node, and a sink node.
******************************************************************************/
BipartiteGraph::BipartiteGraph(int left_size, int right_size) {
  
  source = new Node;
  sink = new Node;
  
  /** Make all the nodes on the left, The source points to all of
      them.  */
  for(int ii = 0; ii < left_size; ii++) {
    Node* node = new Node;
    node->number = ii;
    left.push_back(node);
    source->insert(node);
  }
  
  /** Make all the nodes on the right, each points to sink.  */
  for(int ii = 0; ii < right_size; ii++) {
    Node* node = new Node;
    node->number = ii;
    right.push_back(node);
    node->insert(sink);
  }
}


/** Destructor.  Frees all nodes.  */
BipartiteGraph::~BipartiteGraph() {
  delete source;
  delete sink;
  for (uint ii=0; ii < left.size(); ii++) delete left[ii];
  for (uint ii=0; ii < right.size(); ii++) delete right[ii];
}


/** Connect a node on the left to a node on the right.  */
void BipartiteGraph::connect(int left_index, int right_index) {
  Node *from = left[left_index];
  Node *to = right[right_index];
  from->insert(to);
}


/** Finds a path from the source to the sink using the breath first
    search algorithm.  If the function returns true, there will be a
    path from the sink to the source along the backedges.  */
bool BipartiteGraph::bfs() {
  queue <Node*> q;
  
  /** set all backedges to null.  */
  clear_backedges();
    
  /** Push the root node onto the queue.  */
  q.push(source);

  Node *current;
  while(!q.empty()) { 

    /** Pop the next node from the queue.  */
    current = q.front();
    q.pop();
    
    /** Add all adjacent nodes that haven't been visited onto the
        queue.  */
    Node::iterator nit;
    for (nit = current->begin(); nit != current->end(); nit++) {
      if((*nit)->backedge) continue; // It's been visited

      (*nit)->backedge = current;
      if (*nit == sink) return true; // we've found the sink!
      q.push(*nit);
    }
  }
  
  /** The queue is empty, and the sink is unreachable.  */
  return false;
}


/** Solves the network flow problem for the graph.  */
vector <int> *BipartiteGraph::solve() {
  
  while(bfs()) {
    
    /** Reverse all the edges along the path found by 'bfs.  */
    Node* current = sink;
    while(current != source) {
      reverse(current->backedge, current);
      current = current->backedge;
    }
  }
  
  /** Examine the nodes in the right side to get the solution. They
      should all only point to one node, and that node should be on
      the left.  Thus it's number is the number of the die used for
      that letter.  */
  vector <int> *result = new vector <int>;
  for (uint ii=0; ii < right.size(); ii++) {
    Node* current = right[ii];
    Node* points_to = *current->begin();

    if (points_to == sink || current->size() != 1) return NULL;
    
    result->push_back(points_to->number); // the die number
  }
  
  return result;
}


/** Makes sure 'from doesn't point to 'to, makes 'to point to
    'from.  
******************************************************************************/
inline void BipartiteGraph::reverse(Node *from, Node *to) {
  from->erase(to);
  to->insert(from);
}


/** Sets all backedges in the graph to NULL.  */
void BipartiteGraph::clear_backedges() {
  for (uint ii=0; ii < left.size(); ii++) left[ii]->backedge = NULL;
  for (uint ii=0; ii < right.size(); ii++) right[ii]->backedge = NULL;
  source->backedge = NULL;
  sink->backedge = NULL;
}
