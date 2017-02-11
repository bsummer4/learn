void readLight(ifstream *inputFile, Simulation_System *system);
bool lightIsShell(Light* light);
Light* getLight(Simulation_System *system, string lightName);
Light* makeLight(Simulation_System *system,
	       string name,
	       Road *road1,
	       double duration1,
	       Road *road2,
	       double duration2,
	       double init);
Light* findLight(Simulation_System *system, Road *road);

