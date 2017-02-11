#ifndef _PRODUCT_H
#define _PRODUCT_H
#include <iostream>
#include <set>
using namespace std;

class Product {
  public:
    void Invert();
    void Multiply_Number(double n);
    void Divide_Number(double n);
    void Multiply_Exponential(int n);
    void Divide_Exponential(int n);
    void Multiply_Binom(int n, int k);
    void Divide_Binom(int n, int k);
    void Clear();
    void Print();
    double Calculate_Product();
  protected:
    multiset <double> numerator;
    multiset <double> denominator;
};

#endif
