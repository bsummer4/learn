#include <vector>
#include <iostream>
#include <map>
#include <string>

using namespace std;


class Edge;
class Node;
class PQ;
class Graph;


/** Priority Queue.  Keeps a bunch of edges sorted by smallest weight.
    Can be filtered to contatin only edges that point to nodes that
    aren't in the tree.  
******************************************************************************/
class PQ: public multimap <double, Edge*> {
 public:
  void add_edges(vector <Edge*> edges);
  void filter();
};




/** Store a node in a graph as a name and a list of weighted edges to
    other nodes 
******************************************************************************/
class Edge {
 public:
  Node *from;
  Node *to;
  double weight;
  bool inTree;
  Edge(Node*, Node*, double);
};


/** Store a node in a graph as a name and a list of weighted edges to
    other nodes 
******************************************************************************/
class Node {
 public:
  string name;
  vector <Edge*> edges;
  Node(string);
  void add_edge(Node*, double);
  bool inTree;
};


/** Return the node of the given name if it is in the graph.
    Otherwise create a new node of that name, add it to the graph, and
    return it.
******************************************************************************/
class Graph {
 public:
  multimap <string,Node*> nodes;
  Node* get_node(string name);
  void add_edge(string name1, string name2, double weight);
  vector <Edge*> *min_span_tree();
};


/** Function prototypes for io.  */
Graph* read_graph(istream & input);
void print_solution(vector <Edge*> *sol);
