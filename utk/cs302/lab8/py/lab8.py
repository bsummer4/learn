#!/usr/bin/python
import os
import sys

class Edge:
    def __init__(self, node1, node2, weight):
        self.nodes = (node1, node2)
        self.weight = weight


class Node:

    """ Store a node in a graph as a name and a list of weighted edges to
        other nodes. """

    def __init__(self, name):
        self.name = name
        self.edges = [] # weight-node pairs

    def add_edge(self, node, weight):
        edge = Edge(self, node, weight)
        self.edges.append(edge)


class Graph:

    """ Store a weighted graph as a dictionary of node names to nodes. The
        nodes contain links to the nodes they are connected to. """
    
    def __init__(self):
        self.nodes = {} # name-node pairs
        
    def get_node(self, name):

        """ Return the node of the given name if it is in the graph.
            Otherwise create a new node of that name, add it to the
            graph, and return it. """ 
        
        if name in self.nodes:
            return self.nodes[name]
        
        node = Node(name)
        self.nodes[name] = node
        return node
        

    def add_edge(self, n_name1, n_name2, weight):

        node1 = self.get_node(n_name1)
        node2 = self.get_node(n_name2)
        node1.add_edge(node2, weight)
        node2.add_edge(node1, weight)
        

    def min_span_tree(self):

        def get_first_node():
            x = self.nodes.items()
            x.sort()
            return x[0][1] # first item, node (not name)
        
        def inTree(edge): return edge.nodes[1] in nodes

        def get_closest(edges):

            if len(edges) == 0:
                print "Unconnected graph"
                exit(0)

            closest = edges[0]
            for edge in edges:
                if edge.weight < closest.weight:
                    closest = edge

            return closest
            
        first_node = get_first_node()
        nodes = [first_node]
        edges = []
        queue = first_node.edges
        
        while len(nodes) != len(self.nodes):
            edge = get_closest(queue)
            queue.remove(edge)

            node = edge.nodes[1]
            
            edges.append(edge)
            nodes.append(node)

            queue = queue + node.edges
            queue = filter(lambda e: not inTree(e), queue)
            
        return edges
            
            
def main():
    graph = read_graph(sys.stdin)
    print_solution(graph.min_span_tree())


def print_solution(solution):
    weight = 0
    for edge in solution:
        weight += edge.weight
        print "Edge [%f,%s,%s]"%(edge.weight, edge.nodes[0].name, edge.nodes[1].name)

    print "Total Weight: %f" % weight


def read_graph(file):

    """Lines in file should look like this: node1 node2 weight"""

    result = Graph()
    lines = [line.split() for line in file.readlines()]

    for line in lines:
        if len(line) == 0: continue
        result.add_edge(line[0], line[1], float(line[2]))

    return result

main()
