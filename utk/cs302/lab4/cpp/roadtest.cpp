#include "roadconfig.h"


main(int argc, char **argv)
{
  Simulation_System *ss;
  Road *r;
  Light *l;
  map <string, Road *>::iterator ri;
  map <string, Light *>::iterator li;
  int i, j;

  if (argc != 2) {
    cerr << "usage: roadtest configuration_file\n";
    exit(1);
  }

  ss = new Simulation_System(argv[1]);
   
  // Print out the roads

  i = 0;
  for (ri = ss->roads.begin(); ri != ss->roads.end(); ri++) {
    i++;
    r = ri->second;
    cout << "Road #" << i << ": " << r->name << endl;
    cout << "  Speed Limit: " << r->slimit << endl;
    cout << "  Arrival Rate.  ->: " << r->arr_rates[0] << " <-: " << r->arr_rates[1] << endl;
    cout << "  First Light: " << r->lights[0]->name << endl;
    for (j = 1; j < r->lights.size(); j++) {
      cout <<  "  Next Light: " << r->lights[j]->name << " at distance " << r->distances[j-1] << endl;
    }
    cout << endl;
  }

  // Print out the lights

  i = 0;
  for (li = ss->lights.begin(); li != ss->lights.end(); li++) {
    i++;
    l = li->second;
    cout << "Light #" << i << ": " << l->name << endl;
    for (j = 0; j < 2; j++) {
      cout << "  Road " << j << ": " << l->roads[j]->name << " for " << l->durations[j] << endl;
    }
    cout << "  Initial duration: " << l->init << endl;
    cout << endl;
  }

  // Double-check the pointers in each road.

  for (ri = ss->roads.begin(); ri != ss->roads.end(); ri++) {
    r = ri->second;
    cout << "Double-checking the light pointers in road " << r->name << endl;
    for (i = 0; i < r->lights.size(); i++) {
      li = ss->lights.find(r->lights[i]->name);
      if (li == ss->lights.end()) {
        cerr << "Error: Light " << r->lights[i]->name << " in road " << r->name 
             << " does not have a corresponding entry in ss->lights\n";
        exit(1);
      }
      if (li->second != r->lights[i]) {
        cerr << "Error: Light " << r->lights[i]->name << " in road " << r->name 
             << " is not the same as the light with that name in ss->lights\n";
        exit(1);
      }
    }
  }

  cout << endl;

  // Double-check the pointers in each light.

  for (li = ss->lights.begin(); li != ss->lights.end(); li++) {
    l = li->second;
    cout << "Double-checking the road pointers in light " << l->name << endl;
    for (i = 0; i < 2; i++) {
      ri = ss->roads.find(l->roads[i]->name);
      if (ri == ss->roads.end()) {
        cerr << "Error: Road " << l->roads[i]->name << " in light " << l->name 
             << " does not have a corresponding entry in ss->roads\n";
        exit(1);
      }
      if (ri->second != l->roads[i]) {
        cerr << "Error: Road " << l->roads[i]->name << " in light " << l->name 
             << " is not the same as the road with that name in ss->roads\n";
        exit(1);
      }
    }
  }
}
