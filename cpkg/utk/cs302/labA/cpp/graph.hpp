#include "main.hpp"


class Node;
class BipartiteGraph;


class Node: public set <Node*> {
public:
  int number;
  Node* backedge;
};


class BipartiteGraph {
public:
  BipartiteGraph(int left_size, int right_size);
  ~BipartiteGraph();
  void connect(int left_index, int right_index);
  bool bfs();
  vector <int> *solve();
  
private:
  void connect(Node  *from, Node *to);
  void reverse(Node  *from, Node *to);
  void clear_backedges();
  vector <Node*> left;
  vector <Node*> right;
  Node* source;
  Node* sink;
};

