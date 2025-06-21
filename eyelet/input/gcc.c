#include <stdio.h>

// Print the 'bits'-bit representation of 'value'
void print_binary(int value, int bits) {
    for (int i = bits - 1; i >= 0; --i) {
        printf("%d", (value >> i) & 1);
    }
}

int main() {
    const int MAX = 10;
    const int BITS = 4;  // 4 bits covers 0..10 (0b0000 to 0b1010)

    printf(" i | Gray | Gray (binary)\n");
    printf("---+------+---------------\n");
    for (int i = 0; i <= MAX; ++i) {
        int gray = i ^ (i >> 1);
        printf("%2d |  %2d  | ", i, gray);
        print_binary(gray, BITS);
        printf("\n");
    }

    return 0;
}

