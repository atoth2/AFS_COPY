/**
 * fraction.cpp
 * by Corey Pennycuff
 */

#include "fraction.h"
#include <cmath>

using namespace std;

/***********************
 * IO operators.
 ***********************/
istream& operator>>(istream& lhs, Fraction& rhs) {
  long n, d;
  lhs >> n >> d;
  rhs.set_numerator(n);
  rhs.set_denominator(d);
  return lhs;
}

ostream& operator<<(ostream& lhs, const Fraction& rhs) {
  Fraction temp = rhs;
  temp.reduce();
  if ((temp.get_numerator() < 0 && temp.get_denominator() < 0)
    || (temp.get_numerator() >= 0 && temp.get_denominator() < 0)) {
    lhs << "(" << -temp.get_numerator() << "/" << -temp.get_denominator() << ")";
  }
  else {
    lhs << "(" << temp.get_numerator() << "/" << temp.get_denominator() << ")";
  }
  return lhs;
}

/***********************
 * Constructors.
 ***********************/
Fraction::Fraction() {
  this->numerator = 0;
  this->denominator = 1;
}
Fraction::Fraction(long numerator) {
  this->numerator = numerator;
  this->denominator = 1;
}
Fraction::Fraction(long numerator, long denominator) {
  this->numerator = numerator;
  this->denominator = denominator;
}

/***********************
 * Typecasting operators.
 ***********************/
Fraction::operator double() {
  return (double)this->numerator / this->denominator;
}
Fraction::operator bool() {
  return this->numerator;
}

/***********************
 * Getters and Setters
 ***********************/
long Fraction::get_numerator() const {
  return this->numerator;
}
long Fraction::get_denominator() const {
  return this->denominator;
}
void Fraction::set_numerator(long n) {
  this->numerator = n;
}
void Fraction::set_denominator(long d) {
  this->denominator = d;
}

/***********************
 * Helper functions (not in the .h).
 ***********************/
// Compute the Greatest Common Divisor using Euclid's Algorithm.
long gcd(long a, long b) {
  while (b) {
    long temp = b;
    b = a % b;
    a = temp;
  }
  return abs(a);
}

/***********************
 * The reduce() function.
 ***********************/
Fraction& Fraction::reduce() {
  long factor = gcd(this->numerator, this->denominator);
  if ((this->numerator < 0) && (this->denominator < 0)) {
    factor = -factor;
  }
  this->numerator /= factor;
  this->denominator /= factor;
  return *this;
}

/***********************
 * Logical operators.
 ***********************/
bool operator==(const Fraction& rhs, const Fraction& lhs) {
  Fraction r{rhs}, l{lhs};
  return (r.get_numerator() * l.get_denominator()) == (r.get_denominator() * l.get_numerator());
}

bool operator!=(const Fraction& rhs, const Fraction& lhs) {
  Fraction r{rhs}, l{lhs};
  return (r.get_numerator() * l.get_denominator()) != (r.get_denominator() * l.get_numerator());
}

bool operator<(const Fraction& rhs, const Fraction& lhs) {
  Fraction r{rhs}, l{lhs};
  r.reduce();
  l.reduce();
  return (r.get_numerator() * l.get_denominator()) < (r.get_denominator() * l.get_numerator());
}

bool operator>(const Fraction& rhs, const Fraction& lhs) {
  Fraction r{rhs}, l{lhs};
  r.reduce();
  l.reduce();
  return (r.get_numerator() * l.get_denominator()) > (r.get_denominator() * l.get_numerator());
}

bool operator<=(const Fraction& rhs, const Fraction& lhs) {
  Fraction r{rhs}, l{lhs};
  r.reduce();
  l.reduce();
  return (r.get_numerator() * l.get_denominator()) <= (r.get_denominator() * l.get_numerator());
}

bool operator>=(const Fraction& rhs, const Fraction& lhs) {
  Fraction r{rhs}, l{lhs};
  r.reduce();
  l.reduce();
  return (r.get_numerator() * l.get_denominator()) >= (r.get_denominator() * l.get_numerator());
}

/***********************
 * Mathematical operators.
 ***********************/
Fraction operator+(const Fraction& rhs, const Fraction& lhs) {
  return Fraction((rhs.get_numerator() * lhs.get_denominator()) + (lhs.get_numerator() * rhs.get_denominator()), rhs.get_denominator() * lhs.get_denominator()).reduce();
}

Fraction operator-(const Fraction& rhs, const Fraction& lhs) {
  return rhs + Fraction(-lhs.get_numerator(), lhs.get_denominator());
}

Fraction operator*(const Fraction& rhs, const Fraction& lhs) {
  return Fraction(rhs.get_numerator() * lhs.get_numerator(), rhs.get_denominator() * lhs.get_denominator()).reduce();
}

Fraction operator/(const Fraction& rhs, const Fraction& lhs) {
  return rhs * Fraction(lhs.get_denominator(), lhs.get_numerator());
}

Fraction operator+=(Fraction& rhs, const Fraction& lhs) {
  rhs = rhs + lhs;
  return rhs;
}

Fraction operator-=(Fraction& rhs, const Fraction& lhs) {
  rhs = rhs - lhs;
  return rhs;
}

Fraction operator*=(Fraction& rhs, const Fraction& lhs) {
  rhs = rhs * lhs;
  return rhs;
}

Fraction operator/=(Fraction& rhs, const Fraction& lhs) {
  rhs = rhs / lhs;
  return rhs;
}

/***********************
 * Unary operators.
 ***********************/
// Pre-increment ++.
Fraction operator++(Fraction& f) {
  f += 1;
  return f;
}

// Pre-decrement --.
Fraction operator--(Fraction& f) {
  f -= 1;
  return f;
}

// Post-increment ++.
Fraction operator++(Fraction& f, int ignore) {
  Fraction temp = f;
  f += 1;
  return temp;
}

// Post-decrement --.
Fraction operator--(Fraction& f, int ignore) {
  Fraction temp = f;
  f -= 1;
  return temp;
}

// Negative
Fraction operator-(const Fraction& f) {
  return Fraction(-f.get_numerator(), f.get_denominator());
}
