 /******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab8
  Fall 2008
  graph.cpp

  Methods for the PQ, Edge, Node, and Graph classes.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab8/
******************************************************************************/


#include "lab8.hpp"



/** PQ methods: */


/** Adds a vector of edges to the Priority Queue.  */
void PQ::add_edges(vector <Edge*> edges) {
  double weight;
  
  for (int ii = 0; ii < (int) edges.size(); ii++) {
    weight = edges[ii]->weight;
    this->insert(make_pair(weight, edges[ii]));
  }
}


/** Removes edges that point to nodes that are marked as being in the
    tree.
******************************************************************************/
void PQ::filter() {
  PQ::iterator pit;
  for(pit = this->begin(); pit != this->end(); pit++)
    if (pit->second->to->inTree)
      this->erase(pit);
}



/** Edge methods: */


/** Creats a new edges between two nodes and having a certain weight.
******************************************************************************/
Edge::Edge(Node *node1, Node *node2, double weight_) {
  from = node1;
  to = node2;
  weight = weight_;
  inTree = false;
}



/** Node methods: */


/** Creates a new node with the given name and no edges. */
Node::Node(string name_) {
  name = name_;
  inTree = false;
}


/** Create an edge between us and another node.  */
void Node::add_edge(Node* node, double weight) {
  Edge* edge = new Edge(this, node, weight);
  edges.push_back(edge);
}



/** Graph methods: */


/** Returns a node with the given name.  If there is already a node by
    that name in the graph then return it.  Otherwise create a new
    one, insert it into the graph, and return it.
******************************************************************************/
Node* Graph::get_node(string name) {
  multimap <string,Node*>::iterator nit;
    
  /** Node is already in the graph.  */
  nit = nodes.find(name);
  if (nit != nodes.end())
    return nit->second;
    
  /** Not in the graph;  Make one.  */
  Node* node = new Node(name);
  nodes.insert(make_pair(name, node));
  return node;
}


/** Add an edge between two nodes with a given weight.  Creates the
    nodes if neccessary.
******************************************************************************/
void Graph::add_edge(string name1, string name2, double weight) {
  Node* node1 = get_node(name1);
  Node* node2 = get_node(name2);
  node1->add_edge(node2, weight);
  node2->add_edge(node1, weight);
}


/** Finds a minumum spanning tree for the graph.
******************************************************************************/
vector <Edge*> *Graph::min_span_tree() {
  vector <Edge*> *mst = new vector <Edge*>;
  PQ queue;
  PQ::iterator qit;
    
  /** Get first node */
  Node *first = nodes.begin()->second;
  first->inTree = true;  // mark it
  queue.add_edges(first->edges);

  /** Loop until the mst has the same number of nodes as the graph.
      We check this by comparing the number of edges in the mst to the
      number of nodes in the graph. */
  while(mst->size() != nodes.size() - 1) {
    
    /** Get the closest node to the mst from the queue.  */
    qit = queue.begin();

    /** If the queue is empty, and the mst doesn't span the graph then
	the graph is unconnected.  */
    if(qit == queue.end()) return NULL;
    
    /** Get the closest node from the queue, mark it as being in the
	tree, and add the edge leading to it into the mst.  */
    Node* node = qit->second->to;
    node->inTree = true;
    mst->push_back(qit->second);
    
    /** Add the edges of the newly added node to the queue.  */
    queue.add_edges(node->edges);
    
    /** Remove edges that lead to nodes in the mst.  */
    queue.filter();
  }
    
  return mst;
}
