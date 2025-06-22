/* lldm.c  – Leg-Length Discrepancy Measurement */

#include <stdio.h>
#include <math.h>
#include <stdbool.h>

typedef struct {
    /* ──────────  input triples  ────────── */
    double p1xCm, p1yCm;
    double p2xCm, p2yCm;
    double p3xCm, p3yCm;
    double p4xCm, p4yCm;

    /* ──────────  derived scalars  ────────── */
    double dx12Cm, dy12Cm, dy13Cm, dy24Cm;
    double cL1, dL3m, cL3;
    double pL1x1Cm, pL1x2Cm, pL3x3Cm, pL3x4Cm;
    double dd13Cm, ddy13Cm, dd24Cm, ddy24Cm, ddL13;
    double p5xCm, p5yCm, p6xCm, p6yCm;
    double dx51Cm, dx53Cm, dx62Cm, dx64Cm;
    double dy53Cm, dy64Cm;
    double d53Cm, d64Cm, dCm;

    /* ──────────  alarms  ────────── */
    bool   LLDAlarm;
} Measurement;


/* crunch all rules (mirrors Measurement.compute in Python) */
static void compute_measurement(Measurement *m)
{
    /* basic differences */
    m->dx12Cm = m->p1xCm - m->p2xCm;
    m->dy12Cm = m->p1yCm - m->p2yCm;
    m->dy13Cm = m->p1yCm - m->p3yCm;
    m->dy24Cm = m->p2yCm - m->p4yCm;

    /* slope of L1 and its negative reciprocal */
    m->cL1  = m->dy12Cm / m->dx12Cm;
    m->dL3m = 1.0 / m->cL1;
    m->cL3  = -m->dL3m;

    /* intercept-like helpers */
    m->pL1x1Cm = m->cL1 * m->p1xCm;
    m->pL1x2Cm = m->cL1 * m->p2xCm;
    m->pL3x3Cm = m->cL3 * m->p3xCm;
    m->pL3x4Cm = m->cL3 * m->p4xCm;

    /* diagonal deltas */
    m->dd13Cm  = m->pL1x1Cm - m->pL3x3Cm;
    m->ddy13Cm = m->dd13Cm  - m->dy13Cm;
    m->dd24Cm  = m->pL1x2Cm - m->pL3x4Cm;
    m->ddy24Cm = m->dd24Cm  - m->dy24Cm;

    /* line-slope difference */
    m->ddL13 = m->cL1 - m->cL3;

    /* unknown points p5 & p6 (intersection projections) */
    m->p5xCm  = m->ddy13Cm / m->ddL13;
    m->dx51Cm = m->p5xCm - m->p1xCm;
    m->p5yCm  = m->cL1 * m->dx51Cm + m->p1yCm;

    m->p6xCm  = m->ddy24Cm / m->ddL13;
    m->dx62Cm = m->p6xCm - m->p2xCm;
    m->p6yCm  = m->cL1 * m->dx62Cm + m->p2yCm;

    /* more deltas */
    m->dx53Cm = m->p5xCm - m->p3xCm;
    m->dx64Cm = m->p6xCm - m->p4xCm;
    m->dy53Cm = m->p5yCm - m->p3yCm;
    m->dy64Cm = m->p6yCm - m->p4yCm;

    /* squared sums (Pythagoras) */
    double sdx53Cm2 = m->dx53Cm * m->dx53Cm;
    double sdx64Cm2 = m->dx64Cm * m->dx64Cm;
    double sdy53Cm2 = m->dy53Cm * m->dy53Cm;
    double sdy64Cm2 = m->dy64Cm * m->dy64Cm;
    double ssd53Cm2 = sdx53Cm2 + sdy53Cm2;
    double ssd64Cm2 = sdx64Cm2 + sdy64Cm2;

    /* Euclidean distances */
    m->d53Cm = sqrt(ssd53Cm2);
    m->d64Cm = sqrt(ssd64Cm2);

    /* final diagnostic delta */
    m->dCm = m->d53Cm - m->d64Cm;

    /* alarm test (|dCm| > 1.25 cm) */
    m->LLDAlarm = (m->dCm < -1.25) || (m->dCm > 1.25);
}


/* ──────────  example run (mirrors __main__)  ────────── */
int main(void)
{
    Measurement m = {
        /* initialise only the inputs */
        .p1xCm = 10.1, .p1yCm = 7.8,
        .p2xCm = 45.1, .p2yCm = 5.6,
        .p3xCm =  3.6, .p3yCm = 29.8,
        .p4xCm = 54.7, .p4yCm = 28.5
    };

    compute_measurement(&m);

    printf("dCm      = %+0.3f cm\n", m.dCm);
    printf("LLDAlarm = %s\n", m.LLDAlarm ? "true" : "false");

    return 0;
}

