/**
 * Fraction Struct
 * by Alex Toth 
 */

#include <iostream>

// Define the Fraction struct.
class Fraction {
public:
  // Constructors
  Fraction();
  Fraction(long);
  Fraction(long, long);
  // Type casting
  explicit operator double();
  explicit operator bool();
  // Accessors and Mutators
  long get_numerator() const;
  long get_denominator() const;
  void set_numerator(long);
  void set_denominator(long);
  // Reduce the current Fraction object and return the same object (to allow chaining)
  Fraction& reduce();
private:
  long numerator;
  long denominator;
};

// IO operators.
std::istream& operator>>(std::istream& lhs, Fraction& rhs);
std::ostream& operator<<(std::ostream& lhs, const Fraction& rhs);

// Logical operators.
bool operator==(const Fraction& lhs, const Fraction& rhs);
bool operator!=(const Fraction& lhs, const Fraction& rhs);
bool operator<(const Fraction& lhs, const Fraction& rhs);
bool operator>(const Fraction& lhs, const Fraction& rhs);
bool operator<=(const Fraction& lhs, const Fraction& rhs);
bool operator>=(const Fraction& lhs, const Fraction& rhs);

// Mathematical operators.
Fraction operator+(const Fraction& lhs, const Fraction& rhs);
Fraction operator-(const Fraction& lhs, const Fraction& rhs);
Fraction operator*(const Fraction& lhs, const Fraction& rhs);
Fraction operator/(const Fraction& lhs, const Fraction& rhs);
Fraction operator+=(Fraction& lhs, const Fraction& rhs);
Fraction operator-=(Fraction& lhs, const Fraction& rhs);
Fraction operator*=(Fraction& lhs, const Fraction& rhs);
Fraction operator/=(Fraction& lhs, const Fraction& rhs);

// Unary operators.
// Pre-increment.
Fraction operator++(Fraction& f);
Fraction operator--(Fraction& f);
// Post-increment.
Fraction operator++(Fraction& f, int ignore);
Fraction operator--(Fraction& f, int ignore);
// Negative
Fraction operator-(const Fraction& f);
 *
 * Header file for Fraction class, in which numerator and denominator
 * are stored as longs.  The denominator should never be zero.
 */

#include <iostream>

// Define the Fraction struct.
class Fraction {
public:
  // Constructors
  Fraction();
  Fraction(long);
  Fraction(long, long);
  // Type casting
  explicit operator double();
  explicit operator bool();
  // Accessors and Mutators
  long get_numerator() const;
  long get_denominator() const;
  void set_numerator(long);
  void set_denominator(long);
  // Reduce the current Fraction object and return the same object (to allow chaining)
  Fraction& reduce();
private:
  long numerator;
  long denominator;
};

// IO operators.
std::istream& operator>>(std::istream& lhs, Fraction& rhs);
std::ostream& operator<<(std::ostream& lhs, const Fraction& rhs);

// Logical operators.
bool operator==(const Fraction& lhs, const Fraction& rhs);
bool operator!=(const Fraction& lhs, const Fraction& rhs);
bool operator<(const Fraction& lhs, const Fraction& rhs);
bool operator>(const Fraction& lhs, const Fraction& rhs);
bool operator<=(const Fraction& lhs, const Fraction& rhs);
bool operator>=(const Fraction& lhs, const Fraction& rhs);

// Mathematical operators.
Fraction operator+(const Fraction& lhs, const Fraction& rhs);
Fraction operator-(const Fraction& lhs, const Fraction& rhs);
Fraction operator*(const Fraction& lhs, const Fraction& rhs);
Fraction operator/(const Fraction& lhs, const Fraction& rhs);
Fraction operator+=(Fraction& lhs, const Fraction& rhs);
Fraction operator-=(Fraction& lhs, const Fraction& rhs);
Fraction operator*=(Fraction& lhs, const Fraction& rhs);
Fraction operator/=(Fraction& lhs, const Fraction& rhs);

// Unary operators.
// Pre-increment.
Fraction operator++(Fraction& f);
Fraction operator--(Fraction& f);
// Post-increment.
Fraction operator++(Fraction& f, int ignore);
Fraction operator--(Fraction& f, int ignore);
// Negative
Fraction operator-(const Fraction& f);
