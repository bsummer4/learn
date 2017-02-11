#include <vector>
#include <set>
#include <iostream>
using namespace std;

class Node { public: bool visited; set <int> edges; Node() { visited=0; } };
int rows, cols, cell1, cell2;
vector <Node> nodes;
vector <int> sol;

inline int oneD(int i, int j) { return j + (i * cols); }

bool solve(int i) {
  Node* node = &(nodes[i]);
  
  if(nodes[i].visited) return 0;
  nodes[i].visited = 1;
  
  if(i == rows*cols - 1) { sol.push_back(i); return 1; }
  
  for(set <int>::iterator nit = node->edges.begin(); nit != node->edges.end(); nit++)
    if(solve(*nit)) { sol.insert(sol.begin(), i); return 1; }
  return 0;
}

int main() {
  string tmp;
  cin >> tmp >> rows >> tmp >> cols;
  cout << "ROWS " << rows << " COLS " << cols << endl;
  nodes.resize(rows*cols);
  
  for(int row=0; row<rows; row++)
    for(int col=0; col<cols; col++) {
      Node* node = &(nodes[oneD(row,col)]);
      if (col > 0)      node->edges.insert(oneD(row,   col-1)); // left
      if (row > 0)      node->edges.insert(oneD(row-1, col));   // up
      if (col < cols-1) node->edges.insert(oneD(row,   col+1)); // right
      if (row < rows-1) node->edges.insert(oneD(row+1, col));   // down
    }

  while(cin >> tmp >> cell1 >> cell2) {
    cout << "WALL " << cell1 << " " << cell2 << endl;
    nodes[cell1].edges.erase(cell2);
    nodes[cell2].edges.erase(cell1);
  }
  
  solve(0);
  for(int i=0;i<sol.size();i++) cout << "PATH " << sol[i] << endl;
}
