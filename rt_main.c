#include <stdio.h>

extern double oats_main(double,double);

int main(){ double r = oats_main(1.5, 2.25); printf("result: %f\n", r); return 0; }
