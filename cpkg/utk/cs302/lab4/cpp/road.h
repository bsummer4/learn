void readRoad(ifstream *inputFile, Simulation_System *system);
bool roadIsShell(Road* road);
Road* getRoad(Simulation_System *system, string lightName);
Road* findRoad(Simulation_System *system, Light *light);
Road* makeRoad(Simulation_System *system,
	       string name,
	       double slimit,
	       double arr_rates1,
	       double arr_rates2);

