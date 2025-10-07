#include <stdio.h>

extern double add_oats(double,double);

int main(){ double r = add_oats(1.5, 2.25); printf("result: %f\n", r); return 0; }
