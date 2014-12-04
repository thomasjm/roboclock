
#include <math.h>

#include "theta.h"

double sol1(double a, double b, double c) {
  return atan2((a + pow(a,3) + a*pow(b,2) - a*c - sqrt(-pow(b,2) + 2*pow(a,2)*pow(b,2) - pow(a,4)*pow(b,2) + 2*pow(b,4) -
                                                      2*pow(a,2)*pow(b,4) - pow(b,6) + 2*pow(b,2)*c + 2*pow(a,2)*pow(b,2)*c + 2*pow(b,4)*c - pow(b,2)*pow(c,2)))/
              (2.*(pow(a,2) + pow(b,2))),-(-1 - pow(a,2) - pow(b,2) + pow(a,2)/(pow(a,2) + pow(b,2)) + pow(a,4)/(pow(a,2) + pow(b,2)) +
                                           (pow(a,2)*pow(b,2))/(pow(a,2) + pow(b,2)) + c - (pow(a,2)*c)/(pow(a,2) + pow(b,2)) -
                                           (a*sqrt(-pow(b,2) + 2*pow(a,2)*pow(b,2) - pow(a,4)*pow(b,2) + 2*pow(b,4) - 2*pow(a,2)*pow(b,4) - pow(b,6) +
                                                   2*pow(b,2)*c + 2*pow(a,2)*pow(b,2)*c + 2*pow(b,4)*c - pow(b,2)*pow(c,2)))/(pow(a,2) + pow(b,2)))/(2.*b));
}

double sol2(double a, double b, double c) {
  return atan2((a + pow(a,3) + a*pow(b,2) - a*c + sqrt(-pow(b,2) + 2*pow(a,2)*pow(b,2) - pow(a,4)*pow(b,2) + 2*pow(b,4) -
                                                      2*pow(a,2)*pow(b,4) - pow(b,6) + 2*pow(b,2)*c + 2*pow(a,2)*pow(b,2)*c + 2*pow(b,4)*c - pow(b,2)*pow(c,2)))/
              (2.*(pow(a,2) + pow(b,2))),-(-1 - pow(a,2) - pow(b,2) + pow(a,2)/(pow(a,2) + pow(b,2)) + pow(a,4)/(pow(a,2) + pow(b,2)) +
                                           (pow(a,2)*pow(b,2))/(pow(a,2) + pow(b,2)) + c - (pow(a,2)*c)/(pow(a,2) + pow(b,2)) +
                                           (a*sqrt(-pow(b,2) + 2*pow(a,2)*pow(b,2) - pow(a,4)*pow(b,2) + 2*pow(b,4) - 2*pow(a,2)*pow(b,4) - pow(b,6) +
                                                   2*pow(b,2)*c + 2*pow(a,2)*pow(b,2)*c + 2*pow(b,4)*c - pow(b,2)*pow(c,2)))/(pow(a,2) + pow(b,2)))/(2.*b));
}
