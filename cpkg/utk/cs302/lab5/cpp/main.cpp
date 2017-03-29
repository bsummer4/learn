/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab5
  Fall 2008
  main.cpp

  Handle the command-line argements and initialize then start the raid
  simulation.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab5/
******************************************************************************/


#include "lab5.h"


/** Prints out the usage imformation and complains if one of the arguments was incorrect
******************************************************************************/
void usage(string badArgument) {
  cerr << "usage: raid N seed time beta_of eta_of lambda_lf gamma_r beta_r eta_r gamma_s beta_s eta_s" << endl;
  if(badArgument.length())
    cerr << "Bad " << badArgument << "." << endl;
  exit(1);
}


/** Checks the validity of the arguments, makes a sumulation system
    and runs it.
******************************************************************************/
int main(int argc, char** argv) {
  Simulation* simulation;
  int disks, seed;
  double time, beta_of, eta_of, lambda_lf, gamma_r, beta_r, eta_r,
    gamma_s, beta_s, eta_s;
  
  
  if (argc != 13) usage("");

  /** Converts the types and makes sure it worked.  */
  if(!sscanf(argv[1], "%d", &disks)) usage("N");
  if(!sscanf(argv[2], "%d", &seed)) usage("seed");
  if(!sscanf(argv[3], "%lf", &time)) usage("time");
  if(!sscanf(argv[4], "%lf", &beta_of)) usage("beta_of");
  if(!sscanf(argv[5], "%lf", &eta_of)) usage("eta_of");
  if(!sscanf(argv[6], "%lf", &lambda_lf)) usage("lambda_lf");
  if(!sscanf(argv[7], "%lf", &gamma_r)) usage("gamma_r");
  if(!sscanf(argv[8], "%lf", &beta_r)) usage("beta_r");
  if(!sscanf(argv[9], "%lf", &eta_r)) usage("eta_r");
  if(!sscanf(argv[10], "%lf", &gamma_s)) usage("gamma_s");
  if(!sscanf(argv[11], "%lf", &beta_s)) usage("beta_s");
  if(!sscanf(argv[12], "%lf", &eta_s)) usage("eta_s");

  /** Checks the signs.  */
  if(disks <= 0) usage("N");
  if(time <= 0) usage("time");
  if(beta_of <= 0) usage("beta_of");
  if(beta_r <= 0) usage("beta_r");
  if(beta_s <= 0) usage("beta_s");
  if(lambda_lf <= 0) usage("lambda_lf");
  if(eta_of <= 0) usage("eta_of");
  if(eta_r <= 0) usage("eta_r");
  if(eta_s <= 0) usage("eta_s");
  if(gamma_r < 0) usage("gamma_r");
  if(gamma_s < 0) usage("gamma_s");


  simulation = new Simulation(disks,
			      seed,
			      time,
			      beta_of,
			      eta_of,
			      lambda_lf,
			      gamma_r,
			      beta_r,
			      eta_r,
			      gamma_s,
			      beta_s,
			      eta_s);
  simulation->run();

  return 0;
}
