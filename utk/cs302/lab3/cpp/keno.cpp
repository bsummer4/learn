/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab3
  Fall 2008
  keno.cpp:
  
  Calculates the estimated return of a keno game given a set of
  parameters on standard input.  See the following url for more
  details:
  
  http://www.cs.utk.edu/~cs302/Labs/Lab3/
******************************************************************************/

#include "product.h"

int main () {
  double bet; // amount of money bet
  double totalReturn; // Total amount of return from all possible
		      // number of catches.
  int ballsPicked; // Number of balls picked by the gambler
  Product product;
  bool flag=0; // For conditional printing of the results at the end.
  
  cin >> bet >> ballsPicked;
  
  while(1) {
    int catchBalls;
    double probability, expectedReturn, payout;

    cin >> catchBalls >> payout;
    if(!cin.good()) break;

    // This is an ugly hack that lets the first two output lines be
    // printed only if there is more input following it.  This peice
    // of code would make WAY more sense before the loop.
    if(!flag) {
      printf("Bet: %.2f\n", bet);
      printf("Balls Picked: %d\n", ballsPicked);
      flag = 1; // We have read at least one catchBalls,payout pair.
    }

    /** Probability */
    product.Clear();
    product.Multiply_Binom(80-ballsPicked,20-catchBalls);
    product.Multiply_Binom(ballsPicked,catchBalls);
    product.Divide_Binom(80,20);
    probability = product.Calculate_Product();

    /** Return */
    expectedReturn = probability * payout;

    cout << "  ";
    cout << "Probability of catching " << catchBalls << " ";
    cout << "of " << ballsPicked << ": ";
    cout << probability << " -- ";
    cout << "Expected return: " << expectedReturn << endl;

    totalReturn += expectedReturn;
  }

  if(flag) {
    double netResult = totalReturn - bet;
    printf("Your return per bet: %.2f\n", netResult);
    printf("Normalized: %.2f\n", netResult/bet);
  }
  
  return 0;
}
