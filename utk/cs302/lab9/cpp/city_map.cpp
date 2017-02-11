 /******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab9
  Fall 2008
  dijkstra.cpp

  Implements constructor for the city map.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab9/
******************************************************************************/



#include "city_map.h"
#include <vector>
#include <cmath>

// Just a pair of numbers
class CitySize {
public:
  int streets;
  int avenues;
  CitySize(int streets_, int avenues_) {
    streets = streets_;
    avenues = avenues_;
  }
};


// Creates a road segment from args
Road_Segment *make_road_segment(Road_Type type,
				 int number,
				double distance,
				Intersection *from,
				Intersection *to) {
  
  Road_Segment* result = new Road_Segment;
  result->type = type;
  result->number = number;
  result->distance = distance;
  result->from = from;
  result->to = to;
  return result;
}


// Creates a interrseection from args
Intersection *make_intersection(int street,
				int avenue,
				double x,
				double y,
				double green_street,
				double green_avenue) {
  Intersection *result = new Intersection;
  
  result->street = street;
  result->avenue = avenue;
  result->x = x;
  result->y = y;
  result->green[0] = green_street;
  result->green[1] = green_avenue;
  result->backedge = NULL;
  return result;
}



// Creates a distance between two intersections
double calcDistance(Intersection* from, Intersection* to) {
  return sqrt(pow((to->x - from->x), 2) + pow((to->y - from->y), 2));
}

// How we access the vector
inline int oneD(int x, int y, int cols) { return x + y * cols; }


// Connects two intersections with a road segment
void connect_intersection(vector <Intersection*> *graph, Intersection* from, int street, int avenue, CitySize size, Road_Type type) {

  /** Make sure that index's are within the graph.  */
  if(street >= size.streets || street < 0 ||
     avenue >= size.avenues || avenue < 0) {
    return;
  }

  int number;
  if(type == STREET) number = street;
  if(type == AVENUE) number = avenue;
  Intersection *to = graph->at(oneD(street, avenue, size.streets));
  double distance = calcDistance(from, to);
  
  from->adj.push_back(make_road_segment(type, number, distance, from, to));
}


// Connects two intersections along a street with a road segment
void connect_street(vector <Intersection*> *graph, Intersection* from, CitySize size) {
  int street_num = from->street;
  int avenue_num = from->avenue;

  if (street_num % 5 == 0) {
    connect_intersection(graph, from, street_num, avenue_num+1, size, STREET); // west
    connect_intersection(graph, from, street_num, avenue_num-1, size, STREET); // east
    return;
  }

  if (street_num % 2 == 0) {
    connect_intersection(graph, from, street_num, avenue_num+1, size, STREET); // west
    return;
  }

  connect_intersection(graph, from, street_num, avenue_num-1, size, STREET); // east
}


// Connects two intersections along a avenue with a road segment
void connect_avenue(vector <Intersection*> *graph, Intersection* from, CitySize size) {
  int street_num = from->street;
  int avenue_num = from->avenue;
  
  if(avenue_num % 5 == 0 || avenue_num+1 == size.avenues) {
    connect_intersection(graph, from, street_num+1, avenue_num, size, AVENUE); // north
    connect_intersection(graph, from, street_num-1, avenue_num, size, AVENUE); // south
    return;
  }
  
  if(avenue_num % 2 == 0) {
    connect_intersection(graph, from, street_num+1, avenue_num, size, AVENUE); // north
    return;
  }
  
  connect_intersection(graph, from, street_num-1, avenue_num, size, AVENUE); // south
}


// Connects all intersections in the city with road segments
void connect_intersections(City_Map* self, CitySize size) {
  vector <Intersection*> temp(size.streets * size.avenues);
  list <Intersection*>::iterator lit;
  
  /** Build vector of intersections.  */
  for (lit = self->all.begin(); lit != self->all.end(); lit++) {
    int x = (*lit)->street;
    int y = (*lit)->avenue;
    temp[oneD(x,y,size.streets)] = *lit;
  }
  
  /** For each intersection in the vector.  */
  for (int street = 0; street < size.streets; street++)
    for (int avenue = 0; avenue < size.avenues; avenue++) {
      /** Connect the intersection to the adjacent ones.  */
      connect_street(&temp, temp[oneD(street,avenue,size.streets)], size);
      connect_avenue(&temp, temp[oneD(street,avenue,size.streets)], size);
    }
}


/** Read the intersection information from stdin.  */
CitySize read_intersections(City_Map* self) {
  int st, ave;
  double x, y, gr_st, gr_ave;
  CitySize size(0,0);
  
  while(cin >> st >> ave >> x >> y >> gr_st >> gr_ave) {
    self->all.push_back(make_intersection(st, ave, x, y, gr_st, gr_ave));
    if (st + 1 > size.streets) size.streets = st + 1;
    if (ave + 1 > size.avenues) size.avenues = ave + 1;
  }

  return size;
}


/** Constructor for city map.  reads city info from file and builds a
    graph.  
******************************************************************************/
City_Map::City_Map() {
  list <Intersection*>::iterator lit;
  
  CitySize size = read_intersections(this);
  connect_intersections(this, size);
}

