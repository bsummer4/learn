/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab4
  Fall 2008
  roadconfig.cpp:
  
  Reads road configuration file into the data structures specified in
  roadconfig.h

  When a road/light specifies another road/light that doesn't exist, I
  create a 'shell' road/light as a placeholder.  Then, when the
  road/light is later specified in the file I just add imformation to
  the shell.

  To mark a road/light as a shell, I set a pointer to NULL, or have
  the length of a vector be 0, etc. For specifics see the
  lightIsShell, and roadIsShell functions.
  
  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab4/
******************************************************************************/


#include "roadconfig.h"
#include "road.h"
#include "light.h"
#include <fstream>


void system_test(Simulation_System *system);

Simulation_System::Simulation_System(char *filename) {
  ifstream inputFile(filename);
  string temp;
  
  // A lot of errors don't use this exception system.  This is because
  // Dr. Planks errors are incconsistant.  Some of them say "Error: "
  // at the begging, some don't.  Some of them end with a period, some
  // don't, etc.
  try {
    while(inputFile >> temp) {

      // All of the read* functions read everything up until the next
      // "ROAD" or "LIGHT"
      if (temp == string("ROAD")) readRoad(&inputFile, this);
      else if (temp == string("TL")) readLight(&inputFile, this);
    }
    
    system_test(this);
    
  } catch (string e) {
    cerr << string("Error: ") << e << endl;
    exit(1);
  }
}


// TODO: 

//  # A road specifying a light that does not include the road.
//    for light in current_road->lights:
//      for road in light->roads:
//        if road == current_road: return Good
//    return Bad

//  # A light specifying a road that does not include the light. 
//    for road in current_light->roads:
//      for light in road->lights:
//        if light == current_light: return Good
//    return Bad

/** Tests for problems in the data sctructures.  
******************************************************************************/
void system_test(Simulation_System *system) {

  map <string, Road*>::iterator road_iter = system->roads.begin();;
  while( road_iter != system->roads.end() ) {
    Road* road = road_iter->second;

    /** Light specifies road that does not exist.  */
    if(roadIsShell(road)) {
      string error;
      error += "Light " + findLight(system, road)->name + " ";
      error += "specifies road " + road->name + " which does not exist.";
      throw error;
    }

    /** Road specifies light twice.  */
    vector <Light*>::iterator lightVec_iter = road->lights.begin();
    while(lightVec_iter != road->lights.end()) {
      Light* light = *lightVec_iter;
      
      vector <Light*>::iterator lightVec_iter2 = lightVec_iter;
      while(++lightVec_iter2 != road->lights.end()) {
	if (*lightVec_iter2 == *lightVec_iter) {
	  cerr << "Road " + road->name + " specified light " + light->name + " twice\n";
	  exit(1);
	}
      }

      lightVec_iter++;
    }
    
    road_iter++;
  }
  

  map <string, Light*>::iterator light_iter = system->lights.begin();
  while(light_iter != system->lights.end()) {
    Light* light = light_iter->second;

    /** Road specifies light that desn't exist.  */
    if(lightIsShell(light)) {
      string error;
      error += "Road " + findRoad(system, light)->name + " ";
      error += "specifies light " + light->name;
      error += " which does not exist.";
      throw error;
    }
    
    /** Light specifies a road twice.  */
    if(light->roads[0] == light->roads[1])
      throw "Light " + light->name + " has specified road " + light->roads[0]->name + " twice.";
    
    light_iter++;
  }
}
