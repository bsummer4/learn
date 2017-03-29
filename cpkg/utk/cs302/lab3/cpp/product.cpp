/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab3
  Fall 2008
  product.cpp:
  
  Stores large numbers of multiplications and divisions, delaying the
  actual calculation until it is requested.  Also has methods for
  multipliying and dividing by factorials and binomials.
  
  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab3/
******************************************************************************/
#include "product.h"


/** Flips the numerator and the denominator.
******************************************************************************/
void Product::Invert() {
  numerator.swap(denominator);
}


/** Adds a number to the numerator effectivly mulyiplying that number
    to the end result.
******************************************************************************/
void Product::Multiply_Number(double n) {
  if (n == 1) return;

  multiset <double>::iterator iter = denominator.find(n);

  if (iter != denominator.end())
    denominator.erase(iter);

  else
    numerator.insert(n);
}


/** Adds a number to the denomenator effectivly dividing the end
    result by that number.
******************************************************************************/
void Product::Divide_Number(double n) {
  if (n == 1) return;

  multiset <double>::iterator iter = numerator.find(n);
  if (iter != numerator.end())
    numerator.erase(iter);

  else
    denominator.insert(n);
}


/** Multiplie product by n!.  Calls Multiply_Number on all numbers n
    throught 2.
******************************************************************************/
void Product::Multiply_Exponential(int n) {
  while(n>1) Multiply_Number(n--);
}


/** Divide product by n!.  Calls Divide_Number on all numbers n
    throught 2.
******************************************************************************/
void Product::Divide_Exponential(int n) {
  while(n>1) Divide_Number(n--);
}


/** Multiplies product by (n!)/(k!(n-k)!).
******************************************************************************/
void Product::Multiply_Binom(int n, int k) {
  Multiply_Exponential(n);
  Divide_Exponential(k);
  Divide_Exponential(n-k);
}


/** Multiplies product by k!(n-k)!/n!.
******************************************************************************/
void Product::Divide_Binom(int n, int k) {
  Divide_Exponential(n);
  Multiply_Exponential(k);
  Multiply_Exponential(n-k);
}


/** Sets product to 1 by removing all numbers from the numerator and
    denominator.
******************************************************************************/
void Product::Clear() {
  multiset <double>::iterator iter;

  for(iter = numerator.begin(); iter != numerator.end(); iter++)
    numerator.erase(iter);

  for(iter = denominator.begin(); iter != denominator.end(); iter++)
    denominator.erase(iter);
}


/** Prints all the numbers in the numberator and denominator with '*'
    and '/' delimiting numbers from the numerator and denominator.
******************************************************************************/
void Product::Print() {
  multiset <double>::iterator iter;
  
  /** Numerator */
  if(numerator.empty())
    cout << "1";

  else {
    iter = numerator.begin();
    while(1) {
      cout << (*iter);
      iter++;
      if(iter == numerator.end()) break;
      cout << " * ";
    }
  }
  
  /** Denumerator.  */ 
  if(denominator.begin() == denominator.end())
    cout << " / 1";

  for(iter = denominator.begin(); iter != denominator.end(); iter++)
    cout << " / " << (*iter);
  
  cout << endl;
}


/** Calculates the result; The multiple of all the numbers in the
    numerator divided by all the numbers in the denominator.
******************************************************************************/
double Product::Calculate_Product() {
  double result = 1;
  multiset <double>::iterator iter;

  for(iter = numerator.begin(); iter != numerator.end(); iter++)
    result = result * (*iter);

  for(iter = denominator.begin(); iter != denominator.end(); iter++)
    result = result / (*iter);

  return result;
}
