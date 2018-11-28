/**
 * Homework 7C
 * by Corey Pennycuff
 *
 * This program demonstrates the standard mathematical operators as applied
 * to the Fraction struct.
 */
#include <iostream>
#include <iomanip>
#include "fraction.h"
#include "test.h"

using namespace std;

int main() {
  Fraction f1(-1, -2), f2(-3, 4), f3(-10, 14), f4(3, -4), z, y;

  // Simple operators.
  cout << endl << "Standard Math:" << endl;
  cout << test(tostring(f1+f4), "(-1/4)") << "  " << f1 << " + " << f4 << " = (-1/4)" << endl;
  cout << test(tostring(f1+f2), "(-1/4)") << "  " << f1 << " + " << f2 << " = (-1/4)" << endl;
  cout << test(tostring(f1+3), "(7/2)") << "  " << f1 << " + 3 = (7/2)" << endl;
  cout << test(tostring(f1-f2), "(5/4)") << "  " << f1 << " - " << f2 << " = (5/4)" << endl;
  cout << test(tostring(f1-2), "(-3/2)") << "  " << f1 << " - 2 = (-3/2)" << endl;
  cout << test(tostring(f1*f2), "(-3/8)") << "  " << f1 << " * " << f2 << " = (-3/8)" << endl;
  cout << test(tostring(f2*2), "(-3/2)") << "  " << f2 << " * 2 = (-3/2)" << endl;
  cout << test(tostring(f3/f1), "(-10/7)") << "  " << f3 << " / " << f1 << " = (-10/7)" << endl;
  cout << test(tostring(f3/2), "(-5/14)") << "  " << f3 << " / 2 = (-5/14)" << endl;

  // Mathematical Shorthand
  cout << endl << "Shorthand operators:" << endl;
  z = Fraction(3, 4);
  z += 3;
  cout<<  test(tostring(z), "(15/4)") << "  z = (3/4);  z += 3;  z is (15/4)" << endl;
  z = Fraction(3, 4);
  z -= 3;
  cout << test(tostring(z), "(-9/4)") << "  z = (3/4);  z -= 3;  z is (-9/4)" << endl;
  z = Fraction(3, 4);
  z *= 3;
  cout << test(tostring(z), "(9/4)") << "  z = (3/4);  z *= 3;  z is (9/4)" << endl;
  z = Fraction(3, 4);
  z /= 3;
  cout << test(tostring(z), "(1/4)") << "  z = (3/4);  z /= 3;  z is (1/4)" << endl;

  // Unary Operators
  cout << endl << "Unary operators:" << endl;
  z = Fraction(3,4);
  y = z++;
  cout << test(tostring(y) == "(3/4)" && tostring(z) == "(7/4)") << "  z = (3/4);  y = z++;  y is (3/4),  z is (7/4)" << endl;
  z = Fraction(3,4);
  y = z--;
  cout << test(tostring(y) == "(3/4)" && tostring(z) == "(-1/4)") << "  z = (3/4);  y = z--;  y is (3/4),  z is (-1/4)" << endl;
  z = Fraction(3,4);
  y = ++z;
  cout << test(tostring(y) == "(7/4)" && tostring(z) == "(7/4)") << "  z = (3/4);  y = ++z;  y is (7/4),  z is (7/4)" << endl;
  z = Fraction(3,4);
  y = --z;
  cout << test(tostring(y) == "(-1/4)" && tostring(z) == "(-1/4)") << "  z = (3/4);  y = --z;  y is (-1/4),  z is (-1/4)" << endl;
  z = Fraction(5,7);
  cout << test(tostring(-z), "(-5/7)") << "  z = (5/7);  -z is (-5/7)" << endl;
  cout << test(tostring(-Fraction(3,-9)), "(1/3)") << "  -Fraction(3,-9) is (1/3)" << endl;

  // Use in a loop
  cout << endl << "Use in a loop:" << endl;
  cout << setprecision(19);
  double dtemp = 0;
  Fraction ftemp = 0;
  while (dtemp < 1) {
    dtemp += .01;
  }
  while (ftemp < 1) {
    ftemp += Fraction(1, 100);
  }
  cout << "  100 * .01 =" << endl;
  cout << "          double is not exact: " << dtemp << endl;
  cout << test(tostring(ftemp), "(1/1)") << "    Fraction is exact: " << ftemp << " = " << (double)ftemp << endl;
  return 0;
}

