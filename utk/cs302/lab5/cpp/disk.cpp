/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab5
  Fall 2008
  disk.cpp

  Implements methods for the Disk class which maintatins imformaiton
  about the state of a disk in a simulation.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab5/
******************************************************************************/


#include "lab5.h"

Disk::Disk(int id_,
	   double gamma_of_,
	   double beta_of_,
	   double eta_of_,
	   double lambda_lf_,
	   double gamma_r_,
	   double beta_r_,
	   double eta_r_) {
  
  /** Store argumnets as instance variables.  */
  id = id_;
  gamma_of = gamma_of_;
  beta_of = beta_of_;
  eta_of = eta_of_;
  lambda_lf = lambda_lf_;
  gamma_r = gamma_r_;
  beta_r = beta_r_;
  eta_r = eta_r_;
  
  /** Initialize number of failures.  */
  operationalFailures = 0;
  latentFailures = 0;
}



/** Generate an operational failure event for this disk.  */
Event* Disk::genOperationalFailure(double currentTime) {
  double time = currentTime + myWeibull(gamma_of, beta_of, eta_of);
  return new Event(time, OFAIL_EVENT, this);
}


/** Generate an latent failure event for this disk.  */
Event* Disk::genLatentFailure(double currentTime) {
  double time = currentTime + myExponential(lambda_lf);
  return new Event(time, SFAIL_EVENT, this);
}


/** Generate a repair failure event for this disk.  */
Event *Disk::genRepair(double currentTime) {
  double newTime = currentTime + myWeibull(gamma_r, beta_r, eta_r);
  return new Event(newTime, REPAIR_EVENT, this);
}



/** A bunch of self-explanitory, small methods for making changes to
    the state of the disk:  */

void Disk::fail() { operationalFailures++; }
void Disk::sectorFail() { latentFailures++; }
void Disk::scrub() { latentFailures = 0; }

void Disk::repair() {
  operationalFailures = 0;
  latentFailures = 0;
}

void Disk::ofail() {
  operationalFailures++;
}

void Disk::sfail() {
  latentFailures++;
}
