/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab5
  Fall 2008
  utils.cpp

  Contains two functions for generating different probabillity
  distibutions.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab5/
******************************************************************************/


#include "lab5.h"


/** Genrates a weibull with drand48 given a set of shape
    parameter.
******************************************************************************/
double myWeibull(double gamma, double beta, double eta) {
  double tmp1 = -log(1.0 - drand48());
  double tmp2 = pow(tmp1, 1.0/beta);
  return eta * tmp2 + gamma;
}


/** Genrates a random double with an exponential distribution using
    drand48 given a curve parameter.  
******************************************************************************/
double myExponential(double lambda) {
  double result = (-1.0 * log(1.0 - drand48()) / lambda);
  return  result;
}
