#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>

// ──────────────────────────────────────────────────────────────
// ❶ FACTS  (the stuff after the dots in Prolog)
// ──────────────────────────────────────────────────────────────

// measurements
typedef struct {
    int input1[2];
    int disturbance2[2];
} Measurement1;

typedef struct {
    bool input2;
} Measurement2;

typedef struct {
    int input3;
    int disturbance1;
} Measurement3;

typedef struct {
    int output2;
} Measurement4;

// observations
typedef struct {
    int state1;
} Observation1;

typedef struct {
    bool state2;
} Observation2;

typedef struct {
    int state3;
} Observation3;

// targets
typedef struct {
    int output2;
} Target2;

// Initialize facts
Measurement1 measurement1 = {
    .input1 = {6, 11},
    .disturbance2 = {45, 39}
};

Measurement2 measurement2 = {
    .input2 = true
};

Measurement3 measurement3 = {
    .input3 = 56967,
    .disturbance1 = 35766
};

Measurement4 measurement4 = {
    .output2 = 24
};

Observation1 observation1 = {
    .state1 = 80
};

Observation2 observation2 = {
    .state2 = false
};

Observation3 observation3 = {
    .state3 = 22
};

Target2 target2 = {
    .output2 = 29
};

// ──────────────────────────────────────────────────────────────
// ❷ RULES  (each Prolog clause → one C function)
// ──────────────────────────────────────────────────────────────

/**
 * Implements the two-clause Prolog rule:
 *
 *     measurement10(I,M) :-
 *         measurement1(I,[M1,M2]), M1 < M2, M3 is M2-M1, M is sqrt(M3).
 *
 *     measurement10(I,M1) :-
 *         measurement1(I,[M1,M2]), M1 >= M2.
 *
 * Returns 0.0 and sets `ok` to false if no fact matches.
 */
double measurement10(const char* key, bool* ok) {
    *ok = false;

    if (strcmp(key, "input1") == 0) {
        int m1 = measurement1.input1[0];
        int m2 = measurement1.input1[1];
        *ok = true;

        if (m1 < m2) {
            return sqrt(m2 - m1);
        } else {
            return (double)m1;
        }
    }

    return 0.0;
}

/**
 * Collect every (Actuator, C) pair that satisfies the Prolog query:
 *     ?- control1(_, _).
 */
void control1() {
    bool ok;

    // ── Clause #1  (actuator1) ──────────────────────────────────
    double m1 = measurement10("input1", &ok);

    if (
        ok &&
        measurement2.input2 == true &&
        measurement3.disturbance1 != 0
    ) {
        int d1 = measurement3.disturbance1;

        double c1 = m1 * 19.6;            // proportional part
        double c2 = log10((double)d1);    // compensation part
        double c  = c1 - c2;              // feed-forward control

        printf("actuator1: %.4f\n", c);
    }

    // ── Clause #2  (actuator2) ──────────────────────────────────
    if (
        observation3.state3 &&              // state3 must exist
        measurement4.output2 &&             // output2 must exist
        target2.output2                     // target2 output2 must exist
    ) {
        int p3 = observation3.state3;
        int m4 = measurement4.output2;
        int t2 = target2.output2;

        int e = t2 - m4;                   // error
        if (e != 0) {                      // avoid div-by-zero (Prolog would error out)
            int d = p3 - m4;               // differential error
            double c1 = 5.8 * e;           // proportional part
            double n  = 7.3 / (double)e;   // nonlinear factor
            double c2 = n * d;             // nonlinear differential part
            double c  = c1 + c2;           // P-N-D feedback control

            printf("actuator2: %.4f\n", c);
        }
    }
}

// ──────────────────────────────────────────────────────────────
// ❸ RUN THE SAME QUERY Prolog WOULD
// ──────────────────────────────────────────────────────────────
int main() {
    control1();
    return 0;
}

