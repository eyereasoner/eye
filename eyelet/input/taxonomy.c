/* taxonomy.c
   A direct, algorithmic translation of the Prolog “Deep taxonomy”.
   gcc -std=c11 -Wall -Wextra taxonomy.c -o taxonomy
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

/* Return true iff `x` has Prolog type `label`.                     *
 * Valid labels look like  N123, I17, J5 … (a single letter + number)*/
bool type(const char *x, const char *label)
{
    /* 1. Only the atom "z" ever participates. */
    if (strcmp(x, "z") != 0)
        return false;

    /* 2. Split label into the leading letter and the integer part. */
    char kind = label[0];
    if (kind != 'N' && kind != 'I' && kind != 'J')
        return false;

    /* ensure the rest is a non-negative integer                     */
    for (const char *p = label + 1; *p; ++p)
        if (!isdigit((unsigned char)*p))
            return false;

    int k = atoi(label + 1);

    /* 3. Apply the three generic rules derived from the Prolog file.*/
    if (kind == 'N')                 /* N₀, N₁, … always true for z  */
        return true;

    if ((kind == 'I' || kind == 'J') /* I₁, J₁, … true for z if k≥1   */
        && k >= 1)
        return true;

    return false;
}

/* Small test driver that mimics the Prolog query `?- type(_, 'N1000000').` */
int main(void)
{
    const char *query = "N1000000";
    if (type("z", query))
        printf("yes – z has type %s\n", query);
    else
        printf("no  – no object has type %s\n", query);

    /* A couple of sanity checks                                     */
    printf("z  N0  : %s\n",  type("z", "N0")  ? "true" : "false");
    printf("z  I1  : %s\n",  type("z", "I1")  ? "true" : "false");
    printf("z  J42 : %s\n", type("z", "J42")  ? "true" : "false");
    printf("foo N3 : %s\n", type("foo", "N3") ? "true" : "false");
}
