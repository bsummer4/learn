/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab4
  Fall 2008
  light.cpp:

  Implements functions specific to the light class.  Theses should be
  methods, but I can't modify the header.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab4/
******************************************************************************/


#include "roadconfig.h"
#include "light.h"
#include "road.h"
#include <fstream>


/** Read the information about a light from the configuration file and
    add it to the simulation system.
******************************************************************************/
void readLight(ifstream *inputFile, Simulation_System *system) {
  string name, road1name, road2name;
  double duration1, duration2, init;
  Road *road1, *road2;
  
  *inputFile >> name >> road1name >> duration1;
  *inputFile >> road2name >> duration2 >> init;

  //  cout << "TL" << name << road1name << duration1;
  //cout << road2name << duration2 << init << endl;
  
  if (!inputFile->good()) return;
  
  road1 = getRoad(system, road1name);
  road2 = getRoad(system, road2name);

  makeLight(system, name, road1, duration1, road2, duration2, init);
  //cout << "GOOD LIGHT" << endl;
}


/** Make and return a new light.  If the light is already in the system, throw an
    exception.
******************************************************************************/
Light* makeLight(Simulation_System *system,
		 string name,
		 Road *road1,
		 double duration1,
		 Road *road2,
		 double duration2,
		 double init) {

  Light *result;
  map <string, Light *>::iterator light_iter;
  
  light_iter = system->lights.find(name);
  if(light_iter == system->lights.end()) {
    result = new Light;
    system->lights.insert(make_pair(name, result));
  }
  else {
    result = light_iter->second;
    if (!lightIsShell(result)) {
      cerr << "Light " + name + " specified twice\n";
      exit(1);
    }


  }
  
  result->name = name;
  result->roads[0] = road1;
  result->roads[1] = road2;
  result->durations[0] = duration1;
  result->durations[1] = duration2;
  result->init = init;
  
  //fixme: remove these:
  //cout << name << " - " << road1 << " - " << duration1 << " - ";
  //cout << road2 << " - " << duration2 << " - ";
  //cout << init << endl;

  return result;
}


/** Returns a light.  If the light is in the system already, it
    returns that light.  Otherwise it makes a shell light with name
    and returns that.
******************************************************************************/
Light* getLight(Simulation_System *system, string name) {
  Light *result;
  map <string, Light *>::iterator light_iter;
  
  light_iter = system->lights.find(name);
  if(light_iter == system->lights.end()) {
    result = new Light;
    result->name = name;
    result->roads[0] = NULL; // Mark it as a shell.
    system->lights.insert(make_pair(name, result));
  }
  
  else
    result = light_iter->second;
  
  return result;
}


bool lightIsShell(Light* light) {
  return (light->roads[0] == NULL);
}


/** Finds the light the points to a certain road.
******************************************************************************/
Light* findLight(Simulation_System *system, Road *road) {
  map <string, Light*>::iterator light_iter = system->lights.begin();;
  
  while( light_iter != system->lights.end() ) {
    Light* light = light_iter->second;
    
    if (light->roads[0] == road || light->roads[1] == road)
      return light;
    
    light_iter++;
  }
  
  // This should never happen
  throw string("A ROAD SHELL WAS CREATED BUT NOT PUT IN A LIGHT!");
}
