/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab4
  Fall 2008
  road.cpp:

  Implements functions specific to the road class.  Theses should be
  methods, but I can't modify the header.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab4/
******************************************************************************/


#include "roadconfig.h"
#include "road.h"
#include "light.h"
#include <fstream>


/** Makes a road from information in the file.
******************************************************************************/
void readRoad(ifstream *inputFile, Simulation_System *system) {
  string temp;
  string name;
  double forward, backward, slimit;
  Road* result;
  string light_name; // The name of each light.
  double distance=-1; // The distance for each light.

  /** Get basic road information and make a road with it.  */
  *inputFile >> name >> slimit >> forward >> backward;
  //cout << name << "-" << slimit << "-" << forward << "-" << backward << endl;

  result = getRoad(system, name);
  if(!roadIsShell(result)) {
    cerr << "Road " + name + " specified twice\n";
    exit(1);
  }
  result->name = name;
  result->slimit = slimit;
  result->arr_rates[0] = forward;
  result->arr_rates[1] = backward;


  /** Get light information:  */
  
  /** The first one: no distance.  */
  *inputFile >> temp >> light_name;
  //cout << temp << ":" << light_name << endl;
  if (temp == "END" || !inputFile->good()) {
    cerr << "Road " + name + " has no lights\n";
    exit(1);
  }
  result->lights.push_back(getLight(system, light_name));


  /** Are there any more lights. */
  *inputFile >> temp;
  if (temp == string("END")) {
    //    cout << "GOOD ROAD" << endl;
    return;    
  }
  
  /** The rest of the lights.  */
  while(1) {
    *inputFile >> light_name >> distance;
    Light *light = getLight(system, light_name);
    result->lights.push_back(light);
    result->distances.push_back(distance);
    //cout << temp << ":" << light_name << "_" << distance << endl;    
    /** Get the first word on the next line.  */
    *inputFile >> temp;
    if (temp == string("END")) {
      //cout << "READ LIGHT" << endl;
      break;
    }
  }

  //cout << "GOOD ROAD" << endl;
}


bool roadIsShell(Road* road) {
  return (road->lights.size() == 0);
}


/** Returns a road.  If the road is in the system already, it returns
    the road.  Otherwise it makes a shell road with name and returns
    that.
******************************************************************************/
Road* getRoad(Simulation_System *system, string name) {
  Road *result;
  map <string, Road *>::iterator road_iter;
  
  road_iter = system->roads.find(name);
  if(road_iter == system->roads.end()) {
    result = new Road;
    result->name = name;
    system->roads.insert(make_pair(name, result));
  }
  else
    result = road_iter->second;
  
  return result;
}




/** Finds the road the points to a certain light.
******************************************************************************/
Road* findRoad(Simulation_System *system, Light *light) {
  map <string, Road*>::iterator road_iter = system->roads.begin();;


  while( road_iter != system->roads.end() ) {
    Road* road = road_iter->second;
    
    vector <class Light *>::iterator light_iter;
    light_iter = road->lights.begin();
    while (light_iter != road->lights.end()){
      if (*light_iter == light)
	return road;
      light_iter++;
    }
    road_iter++;
  }
  
  // This should never happen
  throw string("A LIGHT SHELL WAS CREATED BUT NOT PUT IN A ROAD!");
}

