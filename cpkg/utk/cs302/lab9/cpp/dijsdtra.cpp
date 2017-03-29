 /******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab9
  Fall 2008
  dijkstra.cpp

  Implements Dijkstra's algorithm for the city map.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab9/
******************************************************************************/



#include "city_map.h"
#include <vector>
#include <math.h>

/** Wait for light in average case. */
inline double avg(double red, double green) {
  return pow(red, 2) / (2 * (red + green));
}


/** Updates the queue after moving the bfs to a new node. */
inline void update(multimap <double, Intersection*> *queue, Intersection* node, int type) {
  list <Road_Segment *>::iterator lit;
  Road_Segment *segment;
  Intersection *target;
  double time;

  for(lit = node->adj.begin(); lit != node->adj.end(); lit++) {
    segment = *lit;
    target = segment->to;

   // update the nodes best time and add the node to the queue
    double green_time = target->green[segment->type];
    double red_time = target->green[segment->type^1];

    // calculate the time for the current segment 
    switch(type) {
    case 'B':
      time = node->best_time + segment->distance * 120;
      break;
    case 'W':
      time = node->best_time + segment->distance * 120 + red_time;
      break;
    case 'A':
      time = node->best_time + segment->distance * 120 + avg(red_time, green_time);
      break;
    }

    // If it's not an improvment
    if (time > target->best_time) continue;

    // Update the time and backedge
    target->best_time = time;
    target->backedge = segment;
    
    // If not visited, add to queue.
    queue->insert(make_pair(time, target));
  }
  
}


/** Builds the path vector by following the backedges from main.  */
inline void build_path(City_Map* self, Intersection* current) {
  while(current->backedge) {
    self->path.push_front(current->backedge);
    current = current->backedge->from;
  }
}


/** initialize backedges, and best_times, as well as finding
    current(start) and end.
******************************************************************************/
void initialize(Intersection** current, Intersection** end, list <Intersection *> *all) {
  list <Intersection *>::iterator lit;
  
  /** Set all distances to infinity, and all backedges to NULL;  */
  *end = *(all->begin());
  for(lit = all->begin(); lit != all->end(); lit++) {
    //cerr << "intitializing a node... " << endl;
    
    /** Set end to the number with the largest street, avenue nums.  */
    if((*end)->street <= (*lit)->street && (*end)->avenue <= (*lit)->avenue) {
      //cerr << "last node" << endl;
      *end = *lit;
    }
    
    /** Set current to the first node and set it's distance.  */
    if ((*lit)->street == 0 && (*lit)->avenue == 0) {
      //cerr << "found first node" << endl;
      (*current) = *lit;
      (*current)->best_time = 0;
      (*current)->backedge = NULL;
      continue;
    }
    
    (*lit)->best_time = HUGE_VAL;
    (*lit)->backedge = NULL;
  }
}

/** Solve Dijkstra's algorithm.  */
double City_Map::Dijkstra(int type) {
  Intersection *current, *end;
  
  // initialize backedges, and best_times, as well as finding
  // current(start) and end.
  initialize(&current, &end, &all);

  while(true) {
    multimap <double, Intersection*>::iterator mit;
    
    //cerr << current->street << "x" << current->avenue << endl;
    
    // Update distances and backedges from current.adj
    update(&bfsq, current, type);
    if(bfsq.size() == 0) break;
    
    // Get the intersection with the smallest best_time
    mit = bfsq.begin();
    current = mit->second;
    bfsq.erase(mit);
  } 

  // Build path from backedges
  build_path(this, end);
  
  return end->best_time;
}
