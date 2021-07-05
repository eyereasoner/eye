#include <stdio.h>
#include <complex.h>
 
int main() {
    double complex z1 = casin(2);
    printf("[] :c-result \"casin(2) = %f%+f*I\".\n", creal(z1), cimag(z1));
    double complex z2 = csin(casin(2));
    printf("[] :c-result \"csin(casin(2)) = %f%+f*I\".\n", creal(z2), cimag(z2));
    double complex z3 = casin(I);
    printf("[] :c-result \"casin(I) = %f%+f*I\".\n", creal(z3), cimag(z3));
    return 0;
}
