/**
 * Homework 7B
 * by Corey Pennycuff
 *
 * This program demonstrates the standard logical operators as applied
 * to the Fraction struct.
 */
#include <iostream>
#include <iomanip>
#include "fraction.h"
#include "test.h"

using namespace std;

int main() {
  Fraction f1{-1,-2}, f2{-3,4}, f3{-10,14}, f4{3,-4}, f5{1,10}, f6{9,10};

  // Logical Operators
  cout << endl << "Boolean comparisons:" << endl;
  cout << test(true, f2 == f4) << "  " << f2 << " == " << f4 << " is true" << endl;
  cout << test(true, f4 == f4) << "  " << f4 << " == " << f4 << " is true" << endl;
  cout << test(false, f1 == f2) << "  " << f1 << " == " << f2 << " is false" << endl;
  cout << test(true, f1 == Fraction(1,2)) << "  " << f1 << " == Fraction(1,2) is true" << endl;
  cout << test(true, Fraction(-5) == Fraction(5,-1)) << "  " << "Fraction(-5) == Fraction(5,-1) is true" << endl;
  cout << endl;
  cout << test(false, f4 != f4) << "  " << f4 << " != " << f4 << " is false" << endl;
  cout << test(true, f1 != f2) << "  " << f1 << " != " << f2 << " is true" << endl;
  cout << endl;
  cout << test(false, f1 < f2) << "  " << f1 << " < " << f2 << " is false" << endl;
  cout << test(true, f5 < f6) << "  " << f5 << " < " << f6 << " is true" << endl;
  cout << test(false, f2 < f4) << "  " << f2 << " < " << f4 << " is false" << endl;
  cout << endl;
  cout << test(false, f1 <= f2) << "  " << f1 << " <= " << f2 << " is false" << endl;
  cout << test(true, f2 <= f4) << "  " << f2 << " <= " << f4 << " is true" << endl;
  cout << endl;
  cout << test(true, f1 > f2) << "  " << f1 << " > " << f2 << " is true" << endl;
  cout << test(false, f5 > f6) << "  " << f5 << " > " << f6 << " is false" << endl;
  cout << endl;
  cout << test(true, f1 >= f2) << "  " << f1 << " >= " << f2 << " is true" << endl;
  cout << test(true, f3 >= f3) << "  " << f3 << " >= " << f3 << " is true" << endl;
  cout << test(true, Fraction(1,2) < Fraction(3,4)) << "  Fraction(1,-2) < Fraction(3,4) is true" << endl;

  // Implicit conversions
  cout << endl << "Testing implicit conversions:" << endl;
  cout << test(false, Fraction(1,2) == .5) << "  Fraction(1,2) == .5 is false" << endl;
  cout << test(true, (double)Fraction(1,2) == .5) << "  (double)Fraction(1,2) == .5 is true" << endl;
  cout << "  Wait... isn't this a contradiction?" << endl;
  cout << endl;
  cout << test(true, Fraction() == .5) << "  Fraction() == .5 is true" << endl;
  cout << test(false, (double)Fraction() == .5) << "  (double)Fraction() == .5 is false" << endl;
  cout << "  This isn't right, either!" << endl;
  cout << endl;
  cout << test(true, Fraction(1) == 1) << "  Fraction(1) == 1 is true" << endl;
  cout << test(true, Fraction(1,2) < 1) << "  Fraction(1,2) < 1 is true" << endl;
  cout << test(false, Fraction(1,2) < .75) << "  Fraction(1,2) < .75 is false" << endl;
  cout << "  Can you figure out what is happening?" << endl;
  return 0;
}

