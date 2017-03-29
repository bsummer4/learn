#include <stdio.h>
#include <iostream>
#include <string>
#include <vector>
#include <map>
using namespace std;

class Road {
 public:
  string name;
  double slimit;
  double arr_rates[2];
  vector <class Light *> lights;
  vector <double> distances;
};

class Light {
 public:
  string name;
  Road *roads[2];
  double durations[2];
  double init;
};

class Simulation_System {
 public:
  Simulation_System(char *filename);
  map <string, Road *> roads;
  map <string, Light *> lights;
};

