/**
 * Homework 7A
 * by Corey Pennycuff
 *
 * This program demonstrates the basic construction, IO, and type casting
 * functionality of the Fraction struct.
 */

#include <iostream>
#include <sstream>
#include "fraction.h"
#include "test.h"

using namespace std;

int main() {
  
  // Fraction Constructors.
  cout << endl << "Three types of constructors:"<< endl;
  Fraction f1{};
  cout << test(true, f1.get_numerator() == 0 && f1.get_denominator() == 1) << "  An empty Fraction should be created as 0/1"  << endl;
  Fraction f2{5};
  cout << test(true, f2.get_numerator() == 5 && f2.get_denominator() == 1) << "  Fraction(5) is 5/1" << endl;
  Fraction f3{2,7};
  cout << test(true, f3.get_numerator() == 2 && f3.get_denominator() == 7) << "  Fraction(2, 7) is 2/7 " << endl;

  // Typecasting.
  cout << endl << "The constructors are used when casting from an int to a Fraction." << endl;
  cout << "If you created your constructors correctly, then the following two tests" << endl << "should work automatically." << endl;
  cout << test(true, ((Fraction)5).get_numerator() == 5 && ((Fraction)5).get_denominator() == 1) << "  (Fraction)5 is 5/1" << endl;
  cout << "      We only define casting for integers, though (notice that floats get truncated):" << endl;
  cout << test(true, ((Fraction)5.5).get_numerator() == 5 && ((Fraction)5.5).get_denominator() == 1) << "      (Fraction)5.5 is 5/1" << endl;

  cout << endl << "Printing a Fraction to an ostream." << endl;
  cout << test(tostring(f3), "(2/7)") << "  Fraction(2,7) should print as (2/7): " << f3 << endl;
  cout << "        And if a Fraction is negative, the numerator should show as negative:" << endl;
  Fraction f4{3, -4};
  cout << test(tostring(f4), "(-3/4)") << "    f4(3, -4) should print as (-3/4): " << f4 << endl;
  Fraction f5{-7, -35};
  cout << test(tostring(f5), "(1/5)") << "    f5(-7, -35) should print as (1/5): " << f5 << endl;

  // IO
  Fraction f7, f8;
  stringstream s;
  cout << "The following numbers are being inserted into a stream: -1 -2 3 -4" << endl;
  s << "-1 -2 3 -4";
  cout << "Now, we will try to print those values out as two fractions" << endl;
  s >> f7 >> f8;
  cout << test(tostring(f7), "(1/2)") << "  f7 = " << f7 << endl << test(tostring(f8), "(-3/4)") << "  f8 = " << f8 << endl;

  // Testing the reduce() function.
  Fraction f9{3,-9};
  cout << endl << "The Fraction.reduce() function:"<< endl;
  f9.reduce();
  cout << test(tostring(f9), "(-1/3)") << "  f9 prints as (-1/3): " << f9 << endl;
  cout << "        Because Fraction().reduce() returns the object, we can use it in an expression." << endl;
  cout << test(tostring(Fraction(3,-9).reduce()), "(-1/3)") << "  Fraction(3,-9).reduce() prints as (-1/3): " << Fraction(3,-9).reduce() << endl;
  Fraction f6{-10, 14};
  cout << "        Fractions should always print out reduced:" << endl;
  cout << test(tostring(f6), "(-5/7)") << "    f6(-10, 14) should print as (-5/7): " << f6 << endl;

  // Type casting from Fraction
  cout << endl << "Type Casting:" << endl;
  cout << test((double)f7, .5) << "  (double)f7 = " << (double)f7 << endl;
  cout << test((double)f3, (double)2/7) << "  (double)f3 = " << (double)f3 << endl;
  cout << test((double)Fraction(5), 5.0) << "  (double)Fraction(5) = " << (double)Fraction(5) << endl;
  cout << test((double)Fraction(5.5), 5.0) << "  (double)Fraction(5.5) = " << (double)Fraction(5.5) << endl;
  cout << test((bool)Fraction(), false) << "  (bool)Fraction() is false" << endl;
  cout << test((bool)Fraction(1), true) << "  (bool)Fraction(1) is true" << endl;

  return 0;
}

