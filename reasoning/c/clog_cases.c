#include <stdio.h>
#include <complex.h>
 
int main() {
    double complex z1 = clog(-2);
    printf("[] :c-result \"clog(2) = %f%+f*I\".\n", creal(z1), cimag(z1));
    double complex z2 = cexp(clog(-2));
    printf("[] :c-result \"cexp(clog(-2)) = %f%+f*I\".\n", creal(z2), cimag(z2));
    double complex z3 = clog(-I);
    printf("[] :c-result \"clog(-I) = %f%+f*I\".\n", creal(z3), cimag(z3));
    return 0;
}
